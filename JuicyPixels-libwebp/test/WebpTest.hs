module WebpTest where

import Codec.Picture (DynamicImage(..), Image(..), PixelRGB8(..), PixelRGBA8(..), generateImage, pixelAt)
import Codec.Picture.Webp
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM)
import Crypto.Hash.MD5 qualified as MD5
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Internal qualified as BI
import Data.ByteString.Lazy qualified as BL
import Data.Either (isRight)
import Data.List (nub, transpose)
import Data.Vector.Storable qualified as V
import Data.Word (Word64, Word8)
import System.Environment (lookupEnv)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, (@?=))

unit_conformance :: IO ()
unit_conformance = do
  sums <- map md5Entry . lines . filter (/= '\r') . BC.unpack <$> testFile "libwebp_tests.md5"
  let files = nub [file | (name, _) <- sums, let (file, ext) = splitAt (length name - 4) name, ext `elem` [".pam", ".ppm"]]
  length files @?= 96
  forM_ files \file -> do
    decoded <- testFile file >>= decodeWebp options
    case decoded of
      Right (image, _) | Just (ext, netpbm) <- netpbmFile image -> assertEqual file (lookup (file <> ext) sums) (Just $ md5Hex netpbm)
      Right _ -> assertFailure $ file <> ": decoded neither as RGB8 nor as RGBA8"
      Left e -> assertFailure $ file <> ": " <> e
  where
    md5Entry line = let (md5, name) = break (== ' ') line in (drop 2 name, md5)

unit_sources :: IO ()
unit_sources =
  forM_ [("tall-lossless.webp", "tall-lossless.pam", fmap snd . netpbmFile), ("tall-lossy-alpha.webp", "tall-lossy-alpha.pgm", alphaFile)] \(file, source, decoded) -> do
    expected <- testFile source
    testFile file >>= decodeWebp options >>= \case
      Right (image, _) -> assertBool (file <> ": decoded pixels differ from " <> source) $ decoded image == Just expected
      Left e -> assertFailure $ file <> ": " <> e

unit_scaledSources :: IO ()
unit_scaledSources =
  forM_ [("tall-lossless.webp", premultiplied), ("tall-lossy-alpha.webp", \(PixelRGBA8 _ _ _ a) -> [fromIntegral a])] \(file, samplesOf) -> do
    webp <- testFile file
    Right (ImageRGBA8 full, _) <- decodeWebp options webp
    Right (ImageRGBA8 half, _) <- decodeWebp options {outputSize = halfSize} webp
    (imageWidth half, imageHeight half) @?= (imageWidth full `div` 2, imageHeight full `div` 2)
    let average x y = map ((/ 4) . sum) $ transpose [samplesOf $ pixelAt full (2 * x + i) (2 * y + j) | i <- [0, 1], j <- [0, 1]]
        differences = [abs (e - d) | y <- [0 .. imageHeight half - 1], x <- [0 .. imageWidth half - 1], (e, d) <- zip (average x y) (samplesOf $ pixelAt half x y)]
    assertBool (file <> ": scaled samples differ from the averages of the full decode by more than 2") $ all (<= 2) differences
  where
    premultiplied (PixelRGBA8 r g b a) = [fromIntegral c * fromIntegral a / 255 | c <- [r, g, b]] <> [fromIntegral a :: Double]

unit_animation :: IO ()
unit_animation = do
  still <- testFile "test.webp"
  small <- testFile "small_31x13.webp"
  Right (ImageRGB8 frame, _) <- decodeWebp options still
  let webp = animation (160, 150) [((16, 8), (128, 128), still), ((0, 0), (31, 13), small)]
      expected = generateImage (canvasPixel frame) 160 150
      canvasPixel img x y
        | x >= 16 && x < 144 && y >= 8 && y < 136, PixelRGB8 r g b <- pixelAt img (x - 16) (y - 8) = PixelRGBA8 r g b 255
        | otherwise = PixelRGBA8 0 0 0 0
  decodeWebp options webp >>= (@?= Right ("RGBA8", imageData expected)) . fmap (samples . fst)

unit_scaledAnimation :: IO ()
unit_scaledAnimation = do
  small <- testFile "small_31x13.webp"
  let webp = animation (37, 13) [((2, 0), (31, 13), small)]
  Right (ImageRGBA8 canvas, _) <- decodeWebp options {outputSize = const $ Right (19, 7)} webp
  (imageWidth canvas, imageHeight canvas) @?= (19, 7)
  [a | y <- [0 .. 6], x <- [0 .. 18], let PixelRGBA8 _ _ _ a = pixelAt canvas x y] @?= [if x >= 1 && x <= 16 then 255 else 0 | _ <- [0 .. 6 :: Int], x <- [0 .. 18 :: Int]]

unit_metadata :: IO ()
unit_metadata = do
  still <- testFile "test.webp"
  let profile = B.pack $ take 1001 $ cycle [0 .. 255]
      exifData = B.pack $ take 101 $ cycle [255, 254 .. 0]
  decodeWebp options (riff [vp8x 0x28 (128, 128), chunk "ICCP" profile, B.drop 12 still, chunk "EXIF" exifData]) >>= (@?= Right (Metadata (Just profile) (Just exifData))) . fmap snd
  decodeWebp options (riff [vp8x 0 (128, 128), chunk "ICCP" profile, B.drop 12 still, chunk "EXIF" exifData]) >>= (@?= Right (Metadata Nothing Nothing)) . fmap snd
  decodeWebp options still >>= (@?= Right (Metadata Nothing Nothing)) . fmap snd

unit_limits :: IO ()
unit_limits = do
  small <- testFile "small_1x1.webp"
  lossless <- testFile "tall-lossless.webp"
  lossyAlpha <- testFile "tall-lossy-alpha.webp"
  let canvas size = animation size [((0, 0), (1, 1), small)]
  forM_ [(2, canvas (512, 512), "512x512 canvas of a lossy frame"), (25, lossless, "1024x4096 lossless image"), (19, lossyAlpha, "1024x4096 lossy image with alpha")] \(memoryMB, webp, image) -> do
    decodeWebp (DecodeOptions memoryMB Right) webp >>= assertBool (image <> " refused within " <> show memoryMB <> " MiB") . isRight
    decodeWebp (DecodeOptions (memoryMB - 1) Right) webp >>= (@?= Left "image is too large") . fmap (samples . fst)
  decodeWebp options (canvas (16385, 16)) >>= (@?= Left "image side exceeds 16384") . fmap (samples . fst)
  decodeWebp options {maxMemoryMB = 0} small >>= (@?= Left "invalid decode options") . fmap (samples . fst)
  decodeWebp options {outputSize = const $ Left "size refused"} small >>= (@?= Left "size refused") . fmap (samples . fst)
  forM_ [(0, 1), (1, 0), (2, 1), (1, 2)] \size ->
    decodeWebp options {outputSize = const $ Right size} small >>= (@?= Left "invalid decode options") . fmap (samples . fst)
  decodeWebp options {outputSize = const $ Right (4, 16)} lossless >>= assertBool "1024x4096 image refused at 4x16" . isRight
  forM_ [(3, 16), (4, 15)] \size ->
    decodeWebp options {outputSize = const $ Right size} lossless >>= (@?= Left "invalid decode options") . fmap (samples . fst)

unit_memory :: IO ()
unit_memory = do
  small <- testFile "small_1x1.webp"
  let chunks = riff $ [vp8x 0 (1, 1)] <> replicate 1000000 (chunk "ABCD" "") <> [B.drop 12 small]
  forM_ [(huffmanGroups 65536, "65536 Huffman groups", 256, 512), (chunks, "1000000 chunks", 32, 64)] \(webp, image, refusedMB, memoryMB) -> do
    decodeWebp options {maxMemoryMB = refusedMB} webp >>= (@?= Left "image is too large") . fmap (samples . fst)
    decodeWebp options {maxMemoryMB = memoryMB} webp >>= assertBool (image <> " refused within " <> show memoryMB <> " MiB") . isRight

unit_concurrent :: IO ()
unit_concurrent = do
  lossless <- testFile "tall-lossless.webp"
  results <- replicateM 4 newEmptyMVar
  forM_ results \result -> forkIO $ decodeWebp (DecodeOptions 25 Right) lossless >>= putMVar result . isRight
  mapM takeMVar results >>= (@?= replicate 4 True)

unit_malformed :: IO ()
unit_malformed = do
  still <- testFile "test.webp"
  let riffSizeAbove = B.take 4 still <> le 4 (B.length still - 7) <> B.drop 8 still
      notWebp = "RIFF" <> le 4 24 <> "WAVEfmt " <> B.replicate 16 0
      inputs = ["", notWebp, riffSizeAbove] <> [B.take n still | n <- [1 .. B.length still - 1]]
  forM_ inputs \input -> decodeWebp options input >>= (@?= Left "invalid WebP container") . fmap (samples . fst)

testFile :: FilePath -> IO B.ByteString
testFile name = lookupEnv "WEBP_TEST_DATA" >>= maybe (assertFailure "WEBP_TEST_DATA is not set") (\dir -> B.readFile $ dir <> "/" <> name)

options :: DecodeOptions
options = DecodeOptions 256 Right

halfSize :: (Int, Int) -> Either String (Int, Int)
halfSize (w, h) = Right (w `div` 2, h `div` 2)

samples :: DynamicImage -> (String, V.Vector Word8)
samples = \case
  ImageRGB8 Image {imageData} -> ("RGB8", imageData)
  ImageRGBA8 Image {imageData} -> ("RGBA8", imageData)
  _ -> ("other", V.empty)

netpbmFile :: DynamicImage -> Maybe (String, B.ByteString)
netpbmFile = \case
  ImageRGB8 Image {imageWidth, imageHeight, imageData} ->
    Just (".ppm", BC.pack ("P6\n" <> show imageWidth <> " " <> show imageHeight <> "\n255\n") <> bytes imageData)
  ImageRGBA8 Image {imageWidth, imageHeight, imageData} ->
    Just (".pam", BC.pack ("P7\nWIDTH " <> show imageWidth <> "\nHEIGHT " <> show imageHeight <> "\nDEPTH 4\nMAXVAL 255\nTUPLTYPE RGB_ALPHA\nENDHDR\n") <> bytes imageData)
  _ -> Nothing

alphaFile :: DynamicImage -> Maybe B.ByteString
alphaFile = \case
  ImageRGBA8 Image {imageWidth, imageHeight, imageData} ->
    Just $ BC.pack ("P5\n" <> show imageWidth <> " " <> show imageHeight <> "\n255\n") <> bytes (V.generate (imageWidth * imageHeight) \i -> imageData V.! (4 * i + 3))
  _ -> Nothing

bytes :: V.Vector Word8 -> B.ByteString
bytes v = let (pixels, size) = V.unsafeToForeignPtr0 v in BI.fromForeignPtr pixels 0 size

md5Hex :: B.ByteString -> String
md5Hex = BC.unpack . BL.toStrict . BB.toLazyByteString . BB.byteStringHex . MD5.hash

animation :: (Int, Int) -> [((Int, Int), (Int, Int), B.ByteString)] -> B.ByteString
animation size frames = riff $ [vp8x 0x12 size, chunk "ANIM" (le 4 0 <> le 2 0)] <> map anmf frames
  where
    anmf ((x, y), (w, h), still) = chunk "ANMF" $ le 3 (x `div` 2) <> le 3 (y `div` 2) <> le 3 (w - 1) <> le 3 (h - 1) <> le 3 100 <> "\0" <> B.drop 12 still

vp8x :: Word8 -> (Int, Int) -> B.ByteString
vp8x flags (w, h) = chunk "VP8X" $ B.pack [flags, 0, 0, 0] <> le 3 (w - 1) <> le 3 (h - 1)

riff :: [B.ByteString] -> B.ByteString
riff chunks = "RIFF" <> le 4 (4 + B.length body) <> "WEBP" <> body
  where
    body = B.concat chunks

chunk :: B.ByteString -> B.ByteString -> B.ByteString
chunk fourcc payload = fourcc <> le 4 (B.length payload) <> payload <> B.replicate (B.length payload `mod` 2) 0

le :: Int -> Int -> B.ByteString
le n v = B.pack [fromIntegral (v `shiftR` (8 * i)) | i <- [0 .. n - 1]]

huffmanGroups :: Int -> B.ByteString
huffmanGroups groups = riff [chunk "VP8L" $ bits stream <> B.replicate 8 0]
  where
    side = 1024
    stream =
      [(0x2f, 8), (side - 1, 14), (side - 1, 14), (1, 1), (0, 3)]
        <> transform 0
        <> transform 1
        <> [(0, 1), (0, 1), (1, 1), (0, 3), (0, 1)]
        <> byteCode
        <> byteCode
        <> concatMap simpleCode [0, 0, 0]
        <> concat [[(reversed (g .&. 0xff), 8), (reversed (g `shiftR` 8), 8)] | i <- [0 .. (side `div` 4) ^ (2 :: Int) - 1], let g = i `mod` groups]
        <> concatMap (trivialGroup . (.&. 0xff)) [0 .. groups - 1]
    transform t = [(1, 1), (t, 2), (0, 3), (0, 1)] <> trivialGroup 0
    trivialGroup green = concatMap simpleCode [green, 64, 32, 255, 0]
    simpleCode symbol = [(1, 1), (0, 1), (1, 1), (symbol, 8)]
    byteCode =
      [(0, 1), (8, 4)]
        <> [(fromEnum (s == 8 || s == 16), 3) | s <- [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8 :: Int]]
        <> [(1, 1), (2, 3), (42, 6), (0, 1)]
        <> concat (replicate 42 [(1, 1), (3, 2)])
        <> [(1, 1), (0, 2)]
    reversed v = foldl (\r i -> r .|. (((v `shiftR` i) .&. 1) `shiftL` (7 - i))) 0 [0 .. 7]

bits :: [(Int, Int)] -> B.ByteString
bits = B.pack . go (0 :: Word64) (0 :: Int)
  where
    go acc n fields
      | n >= 8 = fromIntegral acc : go (acc `shiftR` 8) (n - 8) fields
      | otherwise = case fields of
          (v, k) : rest -> go (acc .|. (fromIntegral v `shiftL` n)) (n + k) rest
          [] -> [fromIntegral acc | n > 0]

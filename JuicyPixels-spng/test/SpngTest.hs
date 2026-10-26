module SpngTest where

import Codec.Compression.Zlib qualified as Z
import Codec.Picture (DynamicImage(..), Image(..), convertRGB8, convertRGBA8)
import Codec.Picture qualified as JP
import Codec.Picture.Spng
import Control.Monad (forM_)
import Data.Bits (complement, shiftR, testBit, xor)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft, isRight)
import Data.Vector.Storable qualified as V
import Data.Word (Word32, Word64, Word8)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))

unit_header :: IO ()
unit_header = do
  headers <- mapM (\name -> B.readFile (suiteImage name) >>= readPngHeader) ["basn6a16", "basi3p08", "basn0g08"]
  headers @?= map Right [PngHeader 32 32 16 6, PngHeader 32 32 8 3, PngHeader 32 32 8 0]

unit_sameAsJuicyPixels :: IO ()
unit_sameAsJuicyPixels = forM_ suite \(name, transparent) -> do
  bytes <- B.readFile (suiteImage name)
  decoded <- decodePng options bytes
  let reference = either error id $ JP.decodePng bytes
      expected
        | transparent = ImageRGBA8 (convertRGBA8 reference)
        | otherwise = ImageRGB8 (convertRGB8 reference)
  assertBool name $ fmap samples decoded == Right (samples expected)

unit_sameOnEveryPlatform :: IO ()
unit_sameOnEveryPlatform = do
  images <- mapM (\(name, _) -> B.readFile (suiteImage name) >>= decodePng options >>= either assertFailure pure) suite
  foldl (V.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211)) (14695981039346656037 :: Word64) (map (snd . samples) images) @?= 512529522448376465

unit_generated :: IO ()
unit_generated = do
  decoded <- decodePng options $ png [ihdr 2 2 8 0, idat (B.pack [0, 10, 20, 0, 30, 40]), iend]
  fmap samples decoded @?= Right ("RGB8", V.fromList [10, 10, 10, 20, 20, 20, 30, 30, 30, 40, 40, 40])

unit_malformed :: IO ()
unit_malformed = do
  valid <- B.readFile (suiteImage "basn2c08")
  let generated = png [ihdr 16 16 8 0, idat (B.replicate (16 * 17) 0), iend]
      (idat1, idat2) = B.splitAt 3 $ snd $ idat (B.replicate (16 * 17) 0)
      split = png [ihdr 16 16 8 0, ("IDAT", idat1), ("IDAT", idat2), iend]
      huge = png [ihdr 65537 65537 8 0, idat (B.replicate 2 0), iend]
      errors =
        [ (png [ihdr 2048 2048 8 0, idat (B.replicate 2049 0), iend], "IDAT stream too short")
        , (huge, "image width exceeds user limit")
        , (flipByte 32 generated, "invalid chunk checksum")
        , (flipByte (B.length (png [ihdr 16 16 8 0, ("IDAT", idat1)]) - 1) split, "invalid chunk checksum")
        , (png [ihdr 16 16 8 0, iend], "invalid chunk position")
        ]
      truncated = [B.take n valid | n <- [0, 7 .. imageDataEnd - 1]]
      imageDataEnd = B.length valid - 16
  mapM (decodePng options) [generated, split] >>= assertBool "valid generated PNGs" . all isRight
  forM_ errors \(input, err) -> decodePng options input >>= (@?= Left err) . fmap samples
  forM_ truncated \input -> decodePng options input >>= assertBool ("truncated to " <> show (B.length input)) . isLeft
  readPngHeader huge >>= (@?= Left "image width exceeds user limit")

unit_memoryLimit :: IO ()
unit_memoryLimit = do
  let header = png [ihdr 16384 16384 16 6, idat (B.replicate 2 0), iend]
  decodePng (DecodeOptions 1) header >>= (@?= Left "image is too large") . fmap samples
  decodePng (DecodeOptions 0) header >>= (@?= Left "invalid decode options") . fmap samples

suite :: [(String, Bool)]
suite =
  [ ("basn0g08", False)
  , ("basn0g16", False)
  , ("basn4a08", True)
  , ("basn4a16", True)
  , ("basn2c08", False)
  , ("basn2c16", False)
  , ("basn6a08", True)
  , ("basn6a16", True)
  , ("basn3p08", False)
  , ("tbbn3p08", True)
  , ("tp1n3p08", True)
  , ("basi0g08", False)
  , ("basi2c08", False)
  , ("basi3p08", False)
  , ("basi4a08", True)
  , ("basi6a16", True)
  ]

suiteImage :: String -> FilePath
suiteImage name = "cbits/libspng/tests/images/" <> name <> ".png"

options :: DecodeOptions
options = DecodeOptions 64

samples :: DynamicImage -> (String, V.Vector Word8)
samples = \case
  ImageRGB8 Image {imageData} -> ("RGB8", imageData)
  ImageRGBA8 Image {imageData} -> ("RGBA8", imageData)
  _ -> ("other", V.empty)

png :: [(B.ByteString, B.ByteString)] -> B.ByteString
png chunks = "\x89PNG\r\n\x1a\n" <> foldMap chunk chunks
  where
    chunk (name, body) = word32 (fromIntegral $ B.length body) <> name <> body <> word32 (crc32 $ name <> body)

ihdr :: Word32 -> Word32 -> Word8 -> Word8 -> (B.ByteString, B.ByteString)
ihdr width height depth colorType = ("IHDR", word32 width <> word32 height <> B.pack [depth, colorType, 0, 0, 0])

idat :: B.ByteString -> (B.ByteString, B.ByteString)
idat scanlines = ("IDAT", BL.toStrict $ Z.compress $ BL.fromStrict scanlines)

iend :: (B.ByteString, B.ByteString)
iend = ("IEND", "")

word32 :: Word32 -> B.ByteString
word32 = BL.toStrict . BB.toLazyByteString . BB.word32BE

crc32 :: B.ByteString -> Word32
crc32 = complement . B.foldl' (\c b -> iterate step (c `xor` fromIntegral b) !! 8) 0xffffffff
  where
    step c = if testBit c 0 then 0xedb88320 `xor` shiftR c 1 else shiftR c 1

flipByte :: Int -> B.ByteString -> B.ByteString
flipByte i bytes = B.take i bytes <> B.singleton (complement $ B.index bytes i) <> B.drop (i + 1) bytes

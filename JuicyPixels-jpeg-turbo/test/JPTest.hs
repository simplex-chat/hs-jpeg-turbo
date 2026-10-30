{-# OPTIONS_GHC -Wno-orphans #-}

module JPTest where

import Codec.Picture (DynamicImage(..), Image(..), Pixel8, PixelBaseComponent, PixelCMYK8(..), PixelRGB8(..), PixelRGBA8(..), PixelYA8(..), convertRGB8, decodeJpeg, dynamicMap, encodePng, generateImage)
import Codec.Picture.JPEGTurbo qualified as JT
import Control.Exception (ErrorCall(..), bracket, finally, try)
import Control.Monad (forM_, unless)
import Data.Bits (finiteBitSize, xor)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft, isRight)
import Data.Int (Int16)
import Data.Vector.Storable qualified as V
import Data.Word (Word64, Word8)
import Foreign (Ptr, Storable, alloca, castPtr, free, nullPtr, peek, with)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..), CSize(..), CUChar, CUInt(..), CULong(..))
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import TurboJPEG2 qualified as TJ2
import TurboJPEG3 qualified as TJ3

instance JT.EncodableRGB PixelRGBA8 where
  jtPixelFormat _ = TJ2.TJPF_RGBA

instance JT.EncodableRGB PixelYA8 where
  jtPixelFormat _ = TJ2.TJPF_UNKNOWN

unit_rgb :: IO ()
unit_rgb = do
  decoded <- JT.encodeRGB 1 Nothing image >>= either error pure . decodeJpeg
  (dynamicMap imageWidth decoded, dynamicMap imageHeight decoded) @?= (imageWidth image, imageHeight image)

unit_rgbaLength :: IO ()
unit_rgbaLength = do
  encoded <- JT.encodeRGB 100 Nothing rgba
  assertBool "RGBA image encoded" (not (B.null encoded))
  assertRejected "encodeRGB: invalid image data length" $ JT.encodeRGB 100 Nothing (rgba {imageWidth = imageWidth rgba + 1} :: Image PixelRGBA8)
  where
    rgba = generateImage (\x y -> PixelRGBA8 (fromIntegral x) (fromIntegral y) (fromIntegral (x + y)) 255) 37 23

unit_shortBuffer :: IO ()
unit_shortBuffer = assertRejected "encodeRGB: invalid image data length" $ JT.encodeRGB 1 Nothing (image {imageHeight = imageHeight image + 1} :: Image PixelRGB8)

unit_badQuality :: IO ()
unit_badQuality = forM_ ([-1, 101] ++ [2 ^ (32 :: Int) + 50 | finiteBitSize (0 :: Int) > 32]) \q -> assertRejected "encodeRGB: invalid quality" $ JT.encodeRGB q Nothing image

unit_maxDimension :: IO ()
unit_maxDimension = forM_ [(65500, 1), (1, 65500)] \(w, h) -> do
  decoded <- JT.encodeRGB 1 Nothing (blank w h) >>= either error pure . decodeJpeg
  (dynamicMap imageWidth decoded, dynamicMap imageHeight decoded) @?= (w, h)

unit_wideImage :: IO ()
unit_wideImage = assertRejected "encodeRGB: invalid image dimensions" $ JT.encodeRGB 100 Nothing (blank 65501 1)

unit_tallImage :: IO ()
unit_tallImage = assertRejected "encodeRGB: invalid image dimensions" $ JT.encodeRGB 100 Nothing (blank 1 65501)

unit_unknownPixelFormat :: IO ()
unit_unknownPixelFormat = assertRejected "encodeRGB: unknown pixel format" $ JT.encodeRGB 100 Nothing (generateImage (\_ _ -> PixelYA8 0 255) 2 2)

unit_sameAsMozjpeg415 :: IO ()
unit_sameAsMozjpeg415 = do
  jpegs <- sequence [JT.encodeRGB 50 Nothing gradient, JT.encodeRGB 20 Nothing texture, JT.encodeRGB 85 Nothing texture]
  map (\jpeg -> (B.length jpeg, fnv1a jpeg)) jpegs @?= [(421, 671102332627477552), (7518, 926183756610730380), (38918, 3582522977617396525)]
  where
    texture = generateImage (\x y -> PixelRGB8 (fromIntegral (x * y)) (fromIntegral (x * 3 + y * 7)) (fromIntegral ((x * x + y * y) `div` 16))) 320 240
    fnv1a = B.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) (14695981039346656037 :: Word64)

unit_iccProfile :: IO ()
unit_iccProfile = do
  plain <- JT.encodeRGB 90 Nothing image
  readIccProfile plain >>= (@?= "")
  forM_ [1, 65519, 65520, 255 * 65519] \size -> do
    let profile = fst $ B.unfoldrN size (\b -> Just (b, b + 1)) (0 :: Word8)
    jpeg <- JT.encodeRGB 90 (Just profile) image
    readIccProfile jpeg >>= (@?= profile)
    let app2 = markerOffset "\xFF\xE2" jpeg
        markers = (size + 65518) `div` 65519
    B.take app2 jpeg <> B.drop (app2 + size + 18 * markers) jpeg @?= plain
  forM_ [0, 255 * 65519 + 1] \size ->
    assertRejected "encodeRGB: invalid ICC profile length" $ JT.encodeRGB 90 (Just (B.replicate size 0)) image

-- TODO: unit_rgba etc
-- TODO: unit_yuv

unit_header :: IO ()
unit_header = do
  ycbcr <- JT.encodeRGB 100 Nothing image
  rgb <- compress (lossy TJ2.TJSAMP_444 <> [colorspace TJ2.TJCS_RGB]) TJ2.TJPF_RGB image
  gray <- compress (lossy TJ2.TJSAMP_GRAY) TJ2.TJPF_GRAY grayGradient
  ycck <- compress (lossy TJ2.TJSAMP_420) TJ2.TJPF_CMYK cmykGradient
  cmyk <- compress (lossy TJ2.TJSAMP_444 <> [colorspace TJ2.TJCS_CMYK]) TJ2.TJPF_CMYK cmykGradient
  subsamp441 <- compress (lossy TJ3.TJSAMP_441) TJ2.TJPF_RGB image
  let unknownSubsamp = patch (sofOffset ycbcr + 17) [0x21] ycbcr
  headers <- mapM JT.readJpegHeader [ycbcr, rgb, gray, ycck, cmyk, subsamp441, unknownSubsamp]
  headers @?= map (Right . uncurry (JT.JpegHeader 37 23))
    [ (TJ2.TJSAMP_420, TJ2.TJCS_YCbCr)
    , (TJ2.TJSAMP_444, TJ2.TJCS_RGB)
    , (TJ2.TJSAMP_GRAY, TJ2.TJCS_GRAY)
    , (TJ2.TJSAMP_420, TJ2.TJCS_YCCK)
    , (TJ2.TJSAMP_444, TJ2.TJCS_CMYK)
    , (TJ3.TJSAMP_441, TJ2.TJCS_YCbCr)
    , (TJ3.TJSAMP_UNKNOWN, TJ2.TJCS_YCbCr)
    ]

unit_unsupported :: IO ()
unit_unsupported = do
  precision12 <- compressWith tj3Compress12 (lossy TJ2.TJSAMP_420) TJ2.TJPF_RGB 37 23 samples12
  lossless <- compress [(TJ3.TJPARAM_LOSSLESS, 1)] TJ2.TJPF_RGB image
  forM_ [(precision12, "unsupported data precision"), (lossless, "unsupported lossless JPEG")] \(jpeg, err) -> do
    JT.readJpegHeader jpeg >>= (@?= Left err)
    decodeError <$> JT.decodeJpeg options jpeg >>= (@?= Just err)
  where
    samples12 = V.generate (37 * 23 * 3) (fromIntegral . (`mod` 4096) . (* 7))

unit_scales :: IO ()
unit_scales = forM_ [(37, 23), (1001, 999), (65500, 1)] \(w, h) -> do
  jpeg <- JT.encodeRGB 100 Nothing (generateImage (\_ _ -> PixelRGB8 200 100 50) w h)
  forM_ scales \(scale, d) ->
    JT.decodeJpeg options {JT.scale} jpeg >>= \case
      Right (ImageRGB8 Image {imageWidth, imageHeight, imageData}) -> do
        (imageWidth, imageHeight) @?= ((w + d - 1) `div` d, (h + d - 1) `div` d)
        V.length imageData @?= imageWidth * imageHeight * 3
        assertBool "decoded colour differs by more than 2" $ and $ zipWith closeEnough (V.toList imageData) (cycle [200, 100, 50])
      _ -> assertFailure "not decoded as RGB"
  where
    closeEnough a b = abs (fromIntegral a - b :: Int) <= 2

unit_sameAsJuicyPixels :: IO ()
unit_sameAsJuicyPixels = do
  ycbcr <- compress (lossy TJ2.TJSAMP_444) TJ2.TJPF_RGB gradient
  gray <- compress (lossy TJ2.TJSAMP_GRAY) TJ2.TJPF_GRAY grayGradient
  ycck <- compress (lossy TJ2.TJSAMP_444) TJ2.TJPF_CMYK cmykGradient
  cmyk <- compress (lossy TJ2.TJSAMP_444 <> [colorspace TJ2.TJCS_CMYK]) TJ2.TJPF_CMYK cmykGradient
  forM_ [ycbcr, gray, ycck, cmyk] \jpeg -> do
    actual <- either error samples <$> JT.decodeJpeg options jpeg
    let expected = either error samples $ decodeJpeg jpeg
    fst actual @?= fst expected
    assertBool "decoded sample differs by more than 3" $ and $ zipWith closeEnough (snd actual) (snd expected)
  where
    closeEnough a b = abs (fromIntegral a - fromIntegral b :: Int) <= 3

unit_truncated :: IO ()
unit_truncated = do
  jpeg <- JT.encodeRGB 90 Nothing gradient
  let sos = markerOffset "\xFF\xDA" jpeg
      headerEnd = sos + 2 + fromIntegral (B.index jpeg (sos + 2)) * 256 + fromIntegral (B.index jpeg (sos + 3))
  forM_ [2, headerEnd, B.length jpeg `div` 2, B.length jpeg - 1] \size ->
    JT.decodeJpeg options (B.take size jpeg) >>= assertBool "truncated JPEG decoded" . isLeft

unit_malformed :: IO ()
unit_malformed = do
  jpeg <- JT.encodeRGB 100 Nothing image
  let png = BL.toStrict (encodePng image)
      withWidth :: Int -> B.ByteString
      withWidth w = patch (sofOffset jpeg + 7) [fromIntegral (w `div` 256), fromIntegral w] jpeg
  forM_ [B.empty, "not a JPEG", png, "\xFF\xD8\xFF\xD9", withWidth 0, withWidth 65501] \input -> do
    JT.readJpegHeader input >>= assertBool "malformed JPEG header read" . isLeft
    JT.decodeJpeg options input >>= assertBool "malformed JPEG decoded" . isLeft
  JT.readJpegHeader "\xFF\xD8\xFF\xD9" >>= (@?= Left "no image in JPEG")
  JT.readJpegHeader (withWidth 65500) >>= (@?= Right (JT.JpegHeader 65500 23 TJ2.TJSAMP_420 TJ2.TJCS_YCbCr))

unit_limits :: IO ()
unit_limits = do
  progressive <- JT.encodeRGB 90 Nothing large
  baseline <- compress (lossy TJ2.TJSAMP_420) TJ2.TJPF_RGB large
  JT.decodeJpeg options {JT.maxMemoryMB = 1} progressive >>= assertBool "progressive JPEG decoded within 1 MB" . isLeft
  JT.decodeJpeg options {JT.maxMemoryMB = 1} baseline >>= assertBool "baseline JPEG not decoded within 1 MB" . isRight
  jpeg <- JT.encodeRGB 90 Nothing image
  decodeError <$> JT.decodeJpeg options {JT.maxPixels = 37 * 23 - 1} jpeg >>= (@?= Just "image is too large")
  decodeError <$> JT.decodeJpeg options {JT.maxPixels = 37 * 23} jpeg >>= (@?= Nothing)
  forM_ [options {JT.maxPixels = 0}, options {JT.maxMemoryMB = 0}, options {JT.maxMemoryMB = 2048}] \invalid ->
    decodeError <$> JT.decodeJpeg invalid jpeg >>= (@?= Just "invalid decode options")
  decodeError <$> JT.decodeJpeg options {JT.maxMemoryMB = 2047} jpeg >>= (@?= Nothing)
  where
    large = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) (fromIntegral (x + y))) 1000 1000

unit_scanLimit :: IO ()
unit_scanLimit = do
  jpeg <- alloca \jpegPtr -> do
    jpegSize <- progressiveJpeg jpegPtr
    jpegBuf <- peek jpegPtr
    B.packCStringLen (castPtr jpegBuf, fromIntegral jpegSize) `finally` free jpegBuf
  let (headers, scans) = B.breakSubstring sos jpeg
      (dcScan, acScan) = B.breakSubstring sos (B.drop 2 scans)
      manyScans = headers <> sos <> dcScan <> B.concat (replicate 600 (B.take (B.length acScan - 2) acScan)) <> "\xFF\xD9"
  decodeError <$> JT.decodeJpeg options manyScans >>= (@?= Just "Progressive JPEG image has more than 500 scans")
  where
    sos = "\xFF\xDA"

foreign import ccall "progressive_jpeg" progressiveJpeg :: Ptr (Ptr CUChar) -> IO CULong

foreign import ccall "icc_profile" iccProfile :: Ptr CUChar -> CULong -> Ptr (Ptr CUChar) -> IO CUInt

foreign import ccall "turbojpeg.h tj3Compress8" tj3Compress8 :: TJ2.TjHandle TJ2.C -> Ptr Word8 -> CInt -> CInt -> CInt -> TJ2.TJPF -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CInt

foreign import ccall "turbojpeg.h tj3Compress12" tj3Compress12 :: TJ2.TjHandle TJ2.C -> Ptr Int16 -> CInt -> CInt -> CInt -> TJ2.TJPF -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CInt

type Compress a = TJ2.TjHandle TJ2.C -> Ptr a -> CInt -> CInt -> CInt -> TJ2.TJPF -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CInt

compress :: PixelBaseComponent px ~ Word8 => [(TJ3.TJPARAM, CInt)] -> TJ2.TJPF -> Image px -> IO B.ByteString
compress params pixelFormat Image {imageWidth, imageHeight, imageData} = compressWith tj3Compress8 params pixelFormat imageWidth imageHeight imageData

compressWith :: Storable a => Compress a -> [(TJ3.TJPARAM, CInt)] -> TJ2.TJPF -> Int -> Int -> V.Vector a -> IO B.ByteString
compressWith tj3Compress params pixelFormat width height pixels =
  bracket TJ2.tjInitCompress TJ2.tjDestroy \jt -> do
    forM_ params \(param, value) -> TJ3.tj3Set jt param value >>= assertSuccess jt
    V.unsafeWith pixels \srcBuf ->
      with nullPtr \jpegBufPtr ->
        with 0 \jpegSizePtr ->
          flip finally (peek jpegBufPtr >>= TJ2.tjFree) do
            tj3Compress jt srcBuf (fromIntegral width) 0 (fromIntegral height) pixelFormat jpegBufPtr jpegSizePtr >>= assertSuccess jt
            jpegBuf <- peek jpegBufPtr
            jpegSize <- peek jpegSizePtr
            B.packCStringLen (castPtr jpegBuf, fromIntegral jpegSize)

assertSuccess :: TJ2.TjHandle a -> CInt -> Assertion
assertSuccess jt rc = unless (rc == 0) $ TJ2.tjGetErrorStr2 jt >>= peekCString >>= assertFailure

readIccProfile :: B.ByteString -> IO B.ByteString
readIccProfile jpeg = B.useAsCStringLen jpeg \(jpegBuf, jpegSize) -> alloca \iccPtr -> do
  iccSize <- iccProfile (castPtr jpegBuf) (fromIntegral jpegSize) iccPtr
  icc <- peek iccPtr
  B.packCStringLen (castPtr icc, fromIntegral iccSize) `finally` free icc

lossy :: TJ2.TJSAMP -> [(TJ3.TJPARAM, CInt)]
lossy (TJ2.TJSAMP subsamp) = [(TJ3.TJPARAM_QUALITY, 90), (TJ3.TJPARAM_SUBSAMP, subsamp)]

colorspace :: TJ2.TJCS -> (TJ3.TJPARAM, CInt)
colorspace (TJ2.TJCS cs) = (TJ3.TJPARAM_COLORSPACE, cs)

options :: JT.DecodeOptions
options = JT.DecodeOptions {JT.scale = JT.Full, JT.maxPixels = 2 ^ (28 :: Int), JT.maxMemoryMB = 512}

scales :: [(JT.Scale, Int)]
scales = [(JT.Full, 1), (JT.Half, 2), (JT.Quarter, 4), (JT.Eighth, 8)]

decodeError :: Either String a -> Maybe String
decodeError = either Just (const Nothing)

samples :: DynamicImage -> (String, [Word8])
samples = \case
  ImageY8 Image {imageData} -> ("Y8", V.toList imageData)
  ImageCMYK8 Image {imageData} -> ("CMYK8", V.toList imageData)
  decoded -> ("RGB8", V.toList (imageData (convertRGB8 decoded)))

markerOffset :: B.ByteString -> B.ByteString -> Int
markerOffset marker = B.length . fst . B.breakSubstring marker

sofOffset :: B.ByteString -> Int
sofOffset = markerOffset "\xFF\xC2"

patch :: Int -> [Word8] -> B.ByteString -> B.ByteString
patch offset bytes jpeg = B.take offset jpeg <> B.pack bytes <> B.drop (offset + length bytes) jpeg

image :: Image PixelRGB8
image = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) (fromIntegral (x * y))) 37 23

gradient :: Image PixelRGB8
gradient = generateImage (\x y -> PixelRGB8 (fromIntegral (x * 6)) (fromIntegral (y * 10)) (fromIntegral ((x + y) * 4))) 37 23

grayGradient :: Image Pixel8
grayGradient = generateImage (\x y -> fromIntegral (x * 3 + y * 5)) 37 23

cmykGradient :: Image PixelCMYK8
cmykGradient = generateImage (\x y -> PixelCMYK8 (fromIntegral (x * 6)) (fromIntegral (y * 10)) (fromIntegral ((x + y) * 4)) (fromIntegral ((x + y) * 3))) 37 23

blank :: Int -> Int -> Image PixelRGB8
blank = generateImage (\_ _ -> PixelRGB8 0 0 0)

assertRejected :: String -> IO a -> Assertion
assertRejected expected action =
  try action >>= \case
    Left (ErrorCall e) -> e @?= expected
    Right _ -> assertFailure "accepted"

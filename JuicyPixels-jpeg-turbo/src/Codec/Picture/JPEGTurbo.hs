module Codec.Picture.JPEGTurbo
  ( encodeRGB
  , EncodableRGB(..)
  , JpegHeader(..)
  , readJpegHeader
  , Scale(..)
  , DecodeOptions(..)
  , decodeJpeg
  ) where

import Codec.Picture (DynamicImage(..), Image(..), Pixel, PixelBaseComponent, PixelRGB8)
import Control.Exception (bracket, finally)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bits (complement, toIntegralSized)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import Data.Word (Word8)
import Foreign (allocaBytes, castPtr, free, peek, with, nullPtr)
import Foreign.C.String (CStringLen, peekCString)
import Foreign.C.Types (CInt, CULong)
import MozJPEG qualified as Moz
import TurboJPEG2 qualified as TJ2
import TurboJPEG3 qualified as TJ3

class (Pixel px, PixelBaseComponent px ~ Word8) => EncodableRGB px where
  jtPixelFormat :: Image px -> TJ2.TJPF

instance EncodableRGB PixelRGB8 where
  jtPixelFormat _ = TJ2.TJPF_RGB

-- XXX: Use tjCompressFromYUV instead
-- instance EncodableRGB PixelYCbCr8 where
--   jtPixelFormat _ = TJ2.TJPF_RGB

encodeRGB :: EncodableRGB px => Int -> Image px -> IO ByteString
encodeRGB jpegQual image@Image{imageWidth, imageHeight, imageData} = do
  unless (validDimension imageWidth && validDimension imageHeight) $ error "encodeRGB: invalid image dimensions"
  unless (toInteger (V.length imageData) == toInteger imageWidth * toInteger imageHeight * pixelSize) $ error "encodeRGB: invalid image data length"
  unless (0 <= jpegQual && jpegQual <= 100) $ error "encodeRGB: invalid quality"
  V.unsafeWith imageData \bufPtr ->
    with nullPtr \jpegBufPtr ->
      with 0 \jpegSizePtr ->
        allocaBytes Moz.jmsgLengthMax \errorBuf ->
          flip finally (peek jpegBufPtr >>= free) do
            rc <- Moz.jpegTurboEncode (castPtr bufPtr) (fromIntegral imageWidth) (fromIntegral imageHeight) pixelFormat (fromIntegral jpegQual) jpegBufPtr jpegSizePtr errorBuf
            unless (rc == 0) $ peekCString errorBuf >>= error
            jpegBuf <- peek jpegBufPtr
            jpegSize <- peek jpegSizePtr
            B.packCStringLen (castPtr jpegBuf, fromIntegral jpegSize)
  where
    validDimension d = 1 <= d && d <= 65500
    pixelFormat = jtPixelFormat image
    pixelSize = maybe (error "encodeRGB: unknown pixel format") toInteger $ TJ2.tjPixelSize pixelFormat

data JpegHeader = JpegHeader
  { jpegWidth :: Int
  , jpegHeight :: Int
  , jpegSubsamp :: TJ2.TJSAMP
  , jpegColorspace :: TJ2.TJCS
  }
  deriving (Eq, Show)

data Scale = Full | Half | Quarter | Eighth
  deriving (Eq, Show)

data DecodeOptions = DecodeOptions
  { scale :: Scale
  , maxPixels :: Int
  , maxMemoryMB :: Int
  }
  deriving (Eq, Show)

readJpegHeader :: ByteString -> IO (Either String JpegHeader)
readJpegHeader jpeg = withDecompressor jpeg readHeader

decodeJpeg :: DecodeOptions -> ByteString -> IO (Either String DynamicImage)
decodeJpeg DecodeOptions{scale, maxPixels, maxMemoryMB} jpeg
  | maxPixels < 1 || maxMemoryMB < 1 || maxMemoryMB > 2047 = pure $ Left "invalid decode options"
  | otherwise = withDecompressor jpeg \jt jpegBuf -> do
      JpegHeader{jpegWidth, jpegHeight, jpegColorspace} <- readHeader jt jpegBuf
      when (toInteger jpegWidth * toInteger jpegHeight > toInteger maxPixels) $ throwE "image is too large"
      setParam jt TJ3.TJPARAM_MAXMEMORY maxMemoryMB
      let width = scaled jpegWidth
          height = scaled jpegHeight
          decompress = decompressPixels jt jpegBuf width height
      case jpegColorspace of
        TJ2.TJCS_GRAY -> ImageY8 . Image width height <$> decompress TJ2.TJPF_GRAY
        TJ2.TJCS_CMYK -> ImageCMYK8 . Image width height <$> decompress TJ2.TJPF_CMYK
        TJ2.TJCS_YCCK -> ImageCMYK8 . Image width height . V.map complement <$> decompress TJ2.TJPF_CMYK
        _ -> ImageRGB8 . Image width height <$> decompress TJ2.TJPF_RGB
  where
    scaled side = (side + denominator - 1) `div` denominator
    denominator = case scale of
      Full -> 1
      Half -> 2
      Quarter -> 4
      Eighth -> 8

withDecompressor :: ByteString -> (TJ2.TjHandle TJ2.D -> CStringLen -> ExceptT String IO a) -> IO (Either String a)
withDecompressor jpeg action
  | toInteger (B.length jpeg) > toInteger (maxBound :: CULong) = pure $ Left "input is too large"
  | otherwise =
      bracket TJ2.tjInitDecompress TJ2.tjDestroy \jt ->
        BU.unsafeUseAsCStringLen jpeg (runExceptT . action jt)

readHeader :: TJ2.TjHandle TJ2.D -> CStringLen -> ExceptT String IO JpegHeader
readHeader jt (jpegBuf, jpegSize) = do
  setParam jt TJ3.TJPARAM_STOPONWARNING 1
  checkResult jt $ TJ3.tj3DecompressHeader jt (castPtr jpegBuf) (fromIntegral jpegSize)
  width <- getParam TJ3.TJPARAM_JPEGWIDTH
  height <- getParam TJ3.TJPARAM_JPEGHEIGHT
  precision <- getParam TJ3.TJPARAM_PRECISION
  lossless <- getParam TJ3.TJPARAM_LOSSLESS
  arithmetic <- getParam TJ3.TJPARAM_ARITHMETIC
  unless (width >= 1 && height >= 1) $ throwE "no image in JPEG"
  unless (precision == 8) $ throwE "unsupported data precision"
  unless (lossless == 0) $ throwE "unsupported lossless JPEG"
  unless (arithmetic == 0) $ throwE "unsupported arithmetic-coded JPEG"
  JpegHeader (fromIntegral width) (fromIntegral height)
    <$> (TJ2.TJSAMP <$> getParam TJ3.TJPARAM_SUBSAMP)
    <*> (TJ2.TJCS <$> getParam TJ3.TJPARAM_COLORSPACE)
  where
    getParam = liftIO . TJ3.tj3Get jt

decompressPixels :: TJ2.TjHandle TJ2.D -> CStringLen -> Int -> Int -> TJ2.TJPF -> ExceptT String IO (V.Vector Word8)
decompressPixels jt (jpegBuf, jpegSize) width height pixelFormat = do
  pixelSize <- maybe (throwE "unknown pixel format") pure $ TJ2.tjPixelSize pixelFormat
  size <- maybe (throwE "image is too large") pure $ toIntegralSized (toInteger width * toInteger height * toInteger pixelSize)
  pixels <- liftIO $ MV.unsafeNew size
  checkResult jt $ MV.unsafeWith pixels \dstBuf ->
    TJ2.tjDecompress2 jt (castPtr jpegBuf) (fromIntegral jpegSize) (castPtr dstBuf) (fromIntegral width) 0 (fromIntegral height) pixelFormat flags
  liftIO $ V.unsafeFreeze pixels
  where
    flags = TJ2.TJFLAG_STOPONWARNING <> TJ2.TJFLAG_LIMITSCANS

setParam :: TJ2.TjHandle a -> TJ3.TJPARAM -> Int -> ExceptT String IO ()
setParam jt param value = checkResult jt $ TJ3.tj3Set jt param (fromIntegral value)

checkResult :: TJ2.TjHandle a -> IO CInt -> ExceptT String IO ()
checkResult jt action = do
  rc <- liftIO action
  unless (rc == 0) $ liftIO (TJ2.tjGetErrorStr2 jt >>= peekCString) >>= throwE

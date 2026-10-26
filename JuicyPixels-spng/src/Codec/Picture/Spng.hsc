module Codec.Picture.Spng
  ( PngHeader(..)
  , readPngHeader
  , DecodeOptions(..)
  , decodePng
  ) where

import Codec.Picture (DynamicImage(..), Image(..))
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.Bits (toIntegralSized)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BU
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import Data.Word (Word32, Word8)
import Foreign (Ptr, allocaBytes, castPtr, peek, peekByteOff, sizeOf)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..), CSize(..))

#include "spng.h"

data PngHeader = PngHeader
  { pngWidth :: Int
  , pngHeight :: Int
  , pngBitDepth :: Int
  , pngColorType :: Int
  }
  deriving (Eq, Show)

newtype DecodeOptions = DecodeOptions
  { maxMemoryMB :: Int
  }
  deriving (Eq, Show)

readPngHeader :: ByteString -> IO (Either String PngHeader)
readPngHeader png = withDecoder png readHeader

decodePng :: DecodeOptions -> ByteString -> IO (Either String DynamicImage)
decodePng DecodeOptions{maxMemoryMB} png
  | maxMemoryMB < 1 = pure $ Left "invalid decode options"
  | otherwise = withDecoder png \ctx -> do
      PngHeader{pngWidth, pngHeight, pngColorType} <- readHeader ctx
      transparency <- hasTransparency ctx
      if transparency || pngColorType `elem` alphaColorTypes
        then ImageRGBA8 . Image pngWidth pngHeight <$> decodePixels ctx #{const SPNG_FMT_RGBA8}
        else ImageRGB8 . Image pngWidth pngHeight <$> decodePixels ctx #{const SPNG_FMT_RGB8}
  where
    alphaColorTypes = [#{const SPNG_COLOR_TYPE_GRAYSCALE_ALPHA}, #{const SPNG_COLOR_TYPE_TRUECOLOR_ALPHA}]
    decodePixels ctx fmt = do
      size <- query (sizeOf (0 :: CSize)) (spngDecodedImageSize ctx fmt) peek
      pixelBytes <- maybe (throwE "image is too large") pure $ toIntegralSized size
      when (toInteger pixelBytes > toInteger maxMemoryMB * 1048576) $ throwE "image is too large"
      pixels <- liftIO $ MV.unsafeNew pixelBytes
      check $ MV.unsafeWith pixels \buf -> spngDecodeImage ctx (castPtr buf) size fmt #{const SPNG_DECODE_TRNS}
      liftIO $ V.unsafeFreeze pixels

data SpngCtx

withDecoder :: ByteString -> (Ptr SpngCtx -> ExceptT String IO a) -> IO (Either String a)
withDecoder png action =
  bracket (spngCtxNew 0) spngCtxFree \ctx ->
    BU.unsafeUseAsCStringLen png \(buf, len) -> runExceptT do
      check $ spngSetPngBuffer ctx (castPtr buf) (fromIntegral len)
      check $ spngSetImageLimits ctx 16384 16384
      check $ spngSetChunkLimits ctx chunkLimit chunkLimit
      action ctx
  where
    chunkLimit = 64 * 1048576

readHeader :: Ptr SpngCtx -> ExceptT String IO PngHeader
readHeader ctx = query #{size struct spng_ihdr} (spngGetIhdr ctx) \ihdr ->
  PngHeader
    <$> (fromIntegral @Word32 <$> #{peek struct spng_ihdr, width} ihdr)
    <*> (fromIntegral @Word32 <$> #{peek struct spng_ihdr, height} ihdr)
    <*> (fromIntegral @Word8 <$> #{peek struct spng_ihdr, bit_depth} ihdr)
    <*> (fromIntegral @Word8 <$> #{peek struct spng_ihdr, color_type} ihdr)

hasTransparency :: Ptr SpngCtx -> ExceptT String IO Bool
hasTransparency ctx = do
  rc <- liftIO $ allocaBytes #{size struct spng_trns} (spngGetTrns ctx)
  if rc == #{const SPNG_ECHUNKAVAIL} then pure False else True <$ checkCode rc

query :: Int -> (Ptr a -> IO CInt) -> (Ptr a -> IO b) -> ExceptT String IO b
query size call result = ExceptT $ allocaBytes size \p -> runExceptT do
  check $ call p
  liftIO $ result p

check :: IO CInt -> ExceptT String IO ()
check action = liftIO action >>= checkCode

checkCode :: CInt -> ExceptT String IO ()
checkCode rc = unless (rc == 0) $ liftIO (spngStrerror rc >>= peekCString) >>= throwE

foreign import ccall "spng.h spng_ctx_new" spngCtxNew :: CInt -> IO (Ptr SpngCtx)

foreign import ccall "spng.h spng_ctx_free" spngCtxFree :: Ptr SpngCtx -> IO ()

foreign import ccall "spng.h spng_set_png_buffer" spngSetPngBuffer :: Ptr SpngCtx -> Ptr () -> CSize -> IO CInt

foreign import ccall "spng.h spng_set_image_limits" spngSetImageLimits :: Ptr SpngCtx -> Word32 -> Word32 -> IO CInt

foreign import ccall "spng.h spng_set_chunk_limits" spngSetChunkLimits :: Ptr SpngCtx -> CSize -> CSize -> IO CInt

foreign import ccall "spng.h spng_get_ihdr" spngGetIhdr :: Ptr SpngCtx -> Ptr () -> IO CInt

foreign import ccall "spng.h spng_get_trns" spngGetTrns :: Ptr SpngCtx -> Ptr () -> IO CInt

foreign import ccall "spng.h spng_decoded_image_size" spngDecodedImageSize :: Ptr SpngCtx -> CInt -> Ptr CSize -> IO CInt

foreign import ccall "spng.h spng_decode_image" spngDecodeImage :: Ptr SpngCtx -> Ptr () -> CSize -> CInt -> CInt -> IO CInt

foreign import ccall "spng.h spng_strerror" spngStrerror :: CInt -> IO CString

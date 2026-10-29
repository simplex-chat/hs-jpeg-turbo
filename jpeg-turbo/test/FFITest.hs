module FFITest where

import Control.Exception
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Foreign
import Foreign.C.String
import Foreign.C.Types
import MozJPEG qualified as Moz
import Test.Tasty.HUnit (assertBool, (@?=))
import TurboJPEG2 qualified as TJ2
import TurboJPEG3 qualified as TJ3

unit_TurboJPEG2 :: IO ()
unit_TurboJPEG2 =
  encoding w h TJ2.TJSAMP_420 100 rgb \encoded ->
    decoding encoded \width height jpegSubsamp _jpegColorspace decoded -> do
      (width, height, jpegSubsamp) @?= (w, h, TJ2.TJSAMP_420)
      assertBool "decoded colour differs by more than 2" $ and $ zipWith closeEnough (B.unpack decoded) (cycle colour)
  where
    w = 37
    h = 23
    colour = [200, 100, 50]
    rgb = B.pack $ concat $ replicate (fromIntegral (w * h)) colour
    pixelFormat = TJ2.TJPF_RGB
    closeEnough a b = abs (fromIntegral a - fromIntegral b :: Int) <= 2

    decoding bsIn action =
      bracket TJ2.tjInitDecompress TJ2.tjDestroy \jt ->
        BU.unsafeUseAsCStringLen bsIn \(bufPtr, bufLen) ->
          alloca \widthPtr ->
            alloca \heightPtr ->
              alloca \jpegSubsampPtr ->
                alloca \jpegColorspacePtr -> do
                  TJ2.tjDecompressHeader3 jt (castPtr bufPtr) (fromIntegral bufLen) widthPtr heightPtr jpegSubsampPtr jpegColorspacePtr >>= \rc ->
                    unless (rc == 0) $ TJ2.tjGetErrorStr2 jt >>= peekCString >>= error
                  width <- peek widthPtr
                  height <- peek heightPtr
                  jpegSubsamp <- peek jpegSubsampPtr
                  jpegColorspace <- peek jpegColorspacePtr
                  pixelSize <- maybe (error "no size for pixelFormat") pure $ TJ2.tjPixelSize pixelFormat
                  let dstSize = fromIntegral $ width * height * pixelSize -- TODO: consider pitch
                  allocaBytes dstSize \dstPtr -> do
                    TJ2.tjDecompress2 jt (castPtr bufPtr) (fromIntegral bufLen) dstPtr width 0 height pixelFormat flags >>= \rc ->
                      unless (rc == 0) $ TJ2.tjGetErrorStr2 jt >>= peekCString >>= error
                    BU.unsafePackCStringLen (castPtr dstPtr, dstSize) >>=
                      action width height jpegSubsamp jpegColorspace
      where
        flags = TJ2.TJFLAG_STOPONWARNING

    encoding width height jpegSubsamp jpegQual bsIn action =
      bracket TJ2.tjInitCompress TJ2.tjDestroy \jt ->
        BU.unsafeUseAsCStringLen bsIn \(bufPtr, _bufLen) ->
          with nullPtr \jpegBufPtr ->
            with 0 \jpegSizePtr -> do
              rc <- TJ2.tjCompress2 jt (castPtr bufPtr) width 0 height pixelFormat jpegBufPtr jpegSizePtr jpegSubsamp jpegQual flags
              jpegBuf <- peek jpegBufPtr
              flip finally (TJ2.tjFree jpegBuf) do
                unless (rc == 0) $ TJ2.tjGetErrorStr2 jt >>= peekCString >>= error
                jpegSize <- peek jpegSizePtr
                BU.unsafePackCStringLen (castPtr jpegBuf, fromIntegral jpegSize) >>=
                  action
      where
        flags = TJ2.TJFLAG_PROGRESSIVE .|. TJ2.TJFLAG_STOPONWARNING

unit_tjBufSize :: IO ()
unit_tjBufSize = toInteger (TJ2.tjBufSize 30000 30000 TJ2.TJSAMP_420) @?= 2700002048

unit_scalingFactors :: IO ()
unit_scalingFactors = do
  factors <- alloca \countPtr -> do
    factorsPtr <- TJ2.tjGetScalingFactors countPtr
    count <- peek countPtr
    peekArray (fromIntegral count) factorsPtr
  assertBool "1/2, 1/4 or 1/8 is missing" $ all (`elem` factors) [TJ2.TjScalingFactor 1 d | d <- [2, 4, 8]]

unit_TurboJPEG3 :: IO ()
unit_TurboJPEG3 =
  bracket TJ2.tjInitDecompress TJ2.tjDestroy \jt -> do
    TJ3.tj3Set jt TJ3.TJPARAM_MAXMEMORY 2047 >>= (@?= 0)
    TJ3.tj3Get jt TJ3.TJPARAM_MAXMEMORY >>= (@?= 2047)
    TJ3.tj3Set jt TJ3.TJPARAM_QUALITY 100 >>= (@?= -1)
    BU.unsafeUseAsCStringLen "\xFF\xD8\xFF\xD9" \(jpegBuf, jpegSize) ->
      TJ3.tj3DecompressHeader jt (castPtr jpegBuf) (fromIntegral jpegSize) >>= (@?= 0)
    TJ3.tj3Get jt TJ3.TJPARAM_JPEGWIDTH >>= (@?= -1)

unit_arithmetic :: IO ()
unit_arithmetic = forM_ [1, 2, 4, 8] \d -> do
  (arithmetic, actual) <- B.readFile "cbits/mozjpeg/testimages/testimgari.jpg" >>= decodeScaled d
  (huffman, expected) <- B.readFile "cbits/mozjpeg/testimages/testimgint.jpg" >>= decodeScaled d
  (arithmetic, huffman) @?= (1, 0)
  assertBool "arithmetic-coded JPEG decoded differently" (actual == expected)

unit_encodeError :: IO ()
unit_encodeError =
  allocaBytes Moz.jmsgLengthMax \errorBuf ->
    with nullPtr \jpegBufPtr ->
      with 0 \jpegSizePtr -> do
        Moz.jpegTurboEncode nullPtr 0 1 TJ2.TJPF_RGB 90 nullPtr 0 jpegBufPtr jpegSizePtr errorBuf >>= (@?= -1)
        peekCString errorBuf >>= (@?= "Empty JPEG image (DNL not supported)")
        peek jpegBufPtr >>= (@?= nullPtr)

decodeScaled :: CInt -> B.ByteString -> IO (CInt, B.ByteString)
decodeScaled d jpeg =
  bracket TJ2.tjInitDecompress TJ2.tjDestroy \jt ->
    BU.unsafeUseAsCStringLen jpeg \(jpegBuf, jpegSize) -> do
      TJ3.tj3DecompressHeader jt (castPtr jpegBuf) (fromIntegral jpegSize) >>= (@?= 0)
      arithmetic <- TJ3.tj3Get jt TJ3.TJPARAM_ARITHMETIC
      width <- scaled <$> TJ3.tj3Get jt TJ3.TJPARAM_JPEGWIDTH
      height <- scaled <$> TJ3.tj3Get jt TJ3.TJPARAM_JPEGHEIGHT
      let size = fromIntegral (width * height * 3)
      allocaBytes size \dstBuf -> do
        TJ2.tjDecompress2 jt (castPtr jpegBuf) (fromIntegral jpegSize) dstBuf width 0 height TJ2.TJPF_RGB (TJ2.TJFLAG_STOPONWARNING .|. TJ2.TJFLAG_LIMITSCANS) >>= (@?= 0)
        (arithmetic,) <$> B.packCStringLen (castPtr dstBuf, size)
  where
    scaled side = (side + d - 1) `div` d

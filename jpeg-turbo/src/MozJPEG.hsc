module MozJPEG where

import Foreign
import Foreign.C
import TurboJPEG2 (TJPF (..))

#include <stdio.h>
#include "jpeglib.h"

foreign import ccall "jpeg_turbo_encode" jpegTurboEncode
  :: Ptr CUChar
  -> CInt
  -> CInt
  -> TJPF
  -> CInt
  -> Ptr CUChar
  -> CUInt
  -> Ptr (Ptr CUChar)
  -> Ptr CSize
  -> CString
  -> IO CInt

jmsgLengthMax :: Int
jmsgLengthMax = #{const JMSG_LENGTH_MAX}

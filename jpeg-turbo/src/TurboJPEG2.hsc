{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | TurboJPEG API v2.1
--
-- <https://rawcdn.githack.com/libjpeg-turbo/libjpeg-turbo/2.1.x/doc/html/group___turbo_j_p_e_g.html>
module TurboJPEG2 where

import Foreign
import Foreign.C

#include "turbojpeg.h"

-- * Common

newtype TjHandle a = Handle (Ptr (TjHandle a))

foreign import ccall "turbojpeg.h tjDestroy" tjDestroy :: (TjHandle a) -> IO ()

foreign import ccall "turbojpeg.h tjGetErrorCode" tjGetErrorCode :: (TjHandle a) -> IO CInt

foreign import ccall "turbojpeg.h tjGetErrorStr2" tjGetErrorStr2 :: (TjHandle a) -> IO CString

-- * Buffers

foreign import ccall "turbojpeg.h tjAlloc" tjAlloc :: CInt -> IO (Ptr CUChar)

foreign import ccall "turbojpeg.h tjFree" tjFree :: Ptr CUChar -> IO ()

-- | The maximum size of the buffer (in bytes) required to hold a JPEG image with the given parameters.
foreign import ccall "turbojpeg.h tjBufSize" tjBufSize
  :: CInt -- ^ width
  -> CInt -- ^ height
  -> TJSAMP -- ^ jpegSubsamp
  -> CULong

-- * Compression

data C

-- | Create a TurboJPEG compressor instance.
foreign import ccall "turbojpeg.h tjInitCompress" tjInitCompress :: IO (TjHandle C)

-- | Compress a packed-pixel RGB, grayscale, or CMYK image into a JPEG image.
foreign import ccall "turbojpeg.h tjCompress2" tjCompress2
  :: TjHandle C
  -> {- const -} Ptr CUChar -- ^ srcBuf
  -> CInt -- ^ width
  -> CInt -- ^ pitch
  -> CInt -- ^ height
  -> TJPF -- ^ pixelFormat
  -> Ptr (Ptr CUChar) -- ^ jpegBuf
  -> Ptr CULong -- ^ jpegSize
  -> TJSAMP -- ^ jpegSubsamp
  -> CInt -- ^ jpegQual
  -> TJFLAG -- ^ flags
  -> IO CInt

-- * Decompression

data D

foreign import ccall "turbojpeg.h tjInitDecompress" tjInitDecompress :: IO (TjHandle D)

foreign import ccall "turbojpeg.h tjDecompressHeader3" tjDecompressHeader3
  :: TjHandle D
  -> {- const -} Ptr CUChar -- ^ jpegBuf
  -> CULong -- ^ jpegSize
  -> Ptr CInt -- ^ width
  -> Ptr CInt -- ^ height
  -> Ptr TJSAMP -- ^ jpegSubsamp
  -> Ptr TJCS -- ^ jpefColorspace
  -> IO CInt

foreign import ccall "turbojpeg.h tjDecompress2" tjDecompress2
  :: TjHandle D
  -> {- const -} Ptr CUChar -- ^ jpegBuf
  -> CULong -- ^ jpegSize
  -> Ptr CUChar -- ^ dstBuf
  -> CInt -- ^ width
  -> CInt -- ^ pitch
  -> CInt -- ^ height
  -> TJPF -- ^ pixelFormat
  -> TJFLAG -- ^ flags
  -> IO CInt

data TjScalingFactor = TjScalingFactor { num, denom :: CInt }
  deriving (Eq, Show)

instance Storable TjScalingFactor where
  sizeOf ~_ = #{size tjscalingfactor}
  alignment ~_ = #{alignment tjscalingfactor}
  poke p TjScalingFactor{num, denom} = do
    #{poke tjscalingfactor, num} p num
    #{poke tjscalingfactor, denom} p denom
  peek p = TjScalingFactor
    <$> (#{peek tjscalingfactor, num} p)
    <*> (#{peek tjscalingfactor, denom} p)

foreign import ccall "turbojpeg.h tjGetScalingFactors" tjGetScalingFactors :: Ptr CInt -> IO (Ptr TjScalingFactor)

-- * Transform

data T

foreign import ccall "turbojpeg.h tjInitTransform" tjInitTransform :: IO (TjHandle T)

data TjTransform = TjTransform
  { r :: TjRegion
  , op :: CInt
  , options :: CInt
  , data_ :: Ptr ()
  , customFilter :: Ptr () -- TODO: FunPtr
  }
  deriving (Eq, Show)

instance Storable TjTransform where
  sizeOf ~_ = #{size tjtransform}
  alignment ~_ = #{alignment tjtransform}
  poke p TjTransform{r, op, options, data_} = do
    #{poke tjtransform, r} p r
    #{poke tjtransform, op} p op
    #{poke tjtransform, options} p options
    #{poke tjtransform, data} p data_
    #{poke tjtransform, customFilter} p nullPtr
  peek p = TjTransform
    <$> (#{peek tjtransform, r} p)
    <*> (#{peek tjtransform, op} p)
    <*> (#{peek tjtransform, options} p)
    <*> (#{peek tjtransform, data} p)
    <*> (#{peek tjtransform, customFilter} p)

-- | Cropping region
data TjRegion = TjRegion { x, y, w, h :: CInt }
  deriving (Eq, Show)

instance Storable TjRegion where
  sizeOf ~_ = #{size tjregion}
  alignment ~_ = #{alignment tjregion}
  poke p TjRegion{x, y, w, h} = do
    #{poke tjregion, x} p x
    #{poke tjregion, y} p y
    #{poke tjregion, w} p w
    #{poke tjregion, h} p h
  peek p = TjRegion
    <$> (#{peek tjregion, x} p)
    <*> (#{peek tjregion, y} p)
    <*> (#{peek tjregion, w} p)
    <*> (#{peek tjregion, h} p)

foreign import ccall "turbojpeg.h tjTransform" tjTransform :: TjHandle T -> {- const -} Ptr CUChar -> CULong -> CInt -> Ptr (Ptr CUChar) -> Ptr CULong -> Ptr TjTransform -> CInt -> IO CInt

-- * Enums

-- ** Colorspaces

newtype TJCS = TJCS CInt
  deriving (Eq, Ord, Show, Bits, Storable)

-- | RGB colorspace.
--
-- When generating the JPEG image, the R, G, and B components in the source image are reordered into image planes, but no colorspace conversion or subsampling is performed. RGB JPEG images can be decompressed to packed-pixel images with any of the extended RGB or grayscale pixel formats, but they cannot be decompressed to planar YUV images.
pattern TJCS_RGB :: TJCS
pattern TJCS_RGB = TJCS #{const TJCS_RGB}

-- | YCbCr colorspace.
--
-- YCbCr is not an absolute colorspace but rather a mathematical transformation of RGB designed solely for storage and transmission. YCbCr images must be converted to RGB before they can be displayed. In the YCbCr colorspace, the Y (luminance) component represents the black & white portion of the original image, and the Cb and Cr (chrominance) components represent the color portion of the original image. Historically, the analog equivalent of this transformation allowed the same signal to be displayed to both black & white and color televisions, but JPEG images use YCbCr primarily because it allows the color data to be optionally subsampled in order to reduce network and disk usage. YCbCr is the most common JPEG colorspace, and YCbCr JPEG images can be generated from and decompressed to packed-pixel images with any of the extended RGB or grayscale pixel formats. YCbCr JPEG images can also be generated from and decompressed to planar YUV images.
pattern TJCS_YCbCr :: TJCS
pattern TJCS_YCbCr = TJCS #{const TJCS_YCbCr}

-- | Grayscale colorspace.
--
-- The JPEG image retains only the luminance data (Y component), and any color data from the source image is discarded. Grayscale JPEG images can be generated from and decompressed to packed-pixel images with any of the extended RGB or grayscale pixel formats, or they can be generated from and decompressed to planar YUV images.
pattern TJCS_GRAY :: TJCS
pattern TJCS_GRAY = TJCS #{const TJCS_GRAY}

-- | CMYK colorspace.
--
-- When generating the JPEG image, the C, M, Y, and K components in the source image are reordered into image planes, but no colorspace conversion or subsampling is performed. CMYK JPEG images can only be decompressed to packed-pixel images with the CMYK pixel format.
pattern TJCS_CMYK :: TJCS
pattern TJCS_CMYK = TJCS #{const TJCS_CMYK}

-- | YCCK colorspace.
--
-- YCCK (AKA "YCbCrK") is not an absolute colorspace but rather a mathematical transformation of CMYK designed solely for storage and transmission. It is to CMYK as YCbCr is to RGB. CMYK pixels can be reversibly transformed into YCCK, and as with YCbCr, the chrominance components in the YCCK pixels can be subsampled without incurring major perceptual loss. YCCK JPEG images can only be generated from and decompressed to packed-pixel images with the CMYK pixel format.
pattern TJCS_YCCK :: TJCS
pattern TJCS_YCCK = TJCS #{const TJCS_YCCK}

tjcsNames :: [(TJCS, String)]
tjcsNames =
  [ (TJCS_RGB, "TJCS_RGB")
  , (TJCS_YCbCr, "TJCS_YCbCr")
  , (TJCS_GRAY, "TJCS_GRAY")
  , (TJCS_CMYK, "TJCS_CMYK")
  , (TJCS_YCCK, "TJCS_YCCK")
  ]

-- ** Subsampling

-- | Chrominance subsampling options.
newtype TJSAMP = TJSAMP CInt
  deriving (Eq, Ord, Show, Bits, Storable)

-- | 4:4:4 chrominance subsampling (no chrominance subsampling)
--
-- The JPEG or YUV image will contain one chrominance component for every pixel in the source image.
pattern TJSAMP_444 :: TJSAMP
pattern TJSAMP_444 = TJSAMP #{const TJSAMP_444}

-- | 4:2:2 chrominance subsampling
--
-- The JPEG or YUV image will contain one chrominance component for every 2x1 block of pixels in the source image.
pattern TJSAMP_422 :: TJSAMP
pattern TJSAMP_422 = TJSAMP #{const TJSAMP_422}

-- | 4:2:0 chrominance subsampling
--
-- The JPEG or YUV image will contain one chrominance component for every 2x2 block of pixels in the source image.
pattern TJSAMP_420 :: TJSAMP
pattern TJSAMP_420 = TJSAMP #{const TJSAMP_420}

-- | Grayscale.
--
-- The JPEG or YUV image will contain no chrominance components.
pattern TJSAMP_GRAY :: TJSAMP
pattern TJSAMP_GRAY = TJSAMP #{const TJSAMP_GRAY}

-- | 4:4:0 chrominance subsampling
--
-- The JPEG or YUV image will contain one chrominance component for every 1x2 block of pixels in the source image.
--
-- Note: 4:4:0 subsampling is not fully accelerated in libjpeg-turbo.
pattern TJSAMP_440 :: TJSAMP
pattern TJSAMP_440 = TJSAMP #{const TJSAMP_440}

-- | 4:1:1 chrominance subsampling
--
-- The JPEG or YUV image will contain one chrominance component for every 4x1 block of pixels in the source image. All else being equal, a JPEG image with 4:1:1 subsampling is almost exactly the same size as a JPEG image with 4:2:0 subsampling, and in the aggregate, both subsampling methods produce approximately the same perceptual quality. However, 4:1:1 is better able to reproduce sharp horizontal features.
--
-- Note: 4:1:1 subsampling is not fully accelerated in libjpeg-turbo.
pattern TJSAMP_411 :: TJSAMP
pattern TJSAMP_411 = TJSAMP #{const TJSAMP_411}

tjsampNames :: [(TJSAMP, String)]
tjsampNames =
  [ (TJSAMP_444, "TJSAMP_444")
  , (TJSAMP_422, "TJSAMP_422")
  , (TJSAMP_420, "TJSAMP_420")
  , (TJSAMP_GRAY, "TJSAMP_GRAY")
  , (TJSAMP_440, "TJSAMP_440")
  , (TJSAMP_411, "TJSAMP_411")
  ]

tjMCUWidth :: TJSAMP -> Maybe CInt
tjMCUWidth = \case
  -- TJSAMP_444 -> Just #{const tjMCUWidth[TJSAMP_444]} -- BUG: some CCs throw "variable length array declaration cannot have 'static' storage duration"
  TJSAMP_444 -> Just 8 -- no subsampling
  TJSAMP_422 -> Just 16 -- 16x8 for 4:2:@
  TJSAMP_420 -> Just 16 -- 16x16 for 4:2:0
  TJSAMP_GRAY -> Just 8 -- grayscale
  TJSAMP_440 -> Just 8 -- 8x16 for 4:4:0
  TJSAMP_411 -> Just 32 -- 32x8 for 4:1:1
  _ -> Nothing

-- ** Pixel formats

newtype TJPF = TJPF CInt
  deriving (Eq, Ord, Show, Bits, Storable)

-- | RGB pixel format.
--
-- The red, green, and blue components in the image are stored in 3-byte pixels in the order R, G, B from lowest to highest byte address within each pixel.
pattern TJPF_RGB :: TJPF
pattern TJPF_RGB = TJPF #{const TJPF_RGB}

-- | BGR pixel format.
--
-- The red, green, and blue components in the image are stored in 3-byte pixels in the order B, G, R from lowest to highest byte address within each pixel.
pattern TJPF_BGR :: TJPF
pattern TJPF_BGR = TJPF #{const TJPF_BGR}

-- | RGBX pixel format.
--
-- The red, green, and blue components in the image are stored in 4-byte pixels in the order R, G, B from lowest to highest byte address within each pixel. The X component is ignored when compressing/encoding and undefined when decompressing/decoding.
pattern TJPF_RGBX :: TJPF
pattern TJPF_RGBX = TJPF #{const TJPF_RGBX}

-- | BGRX pixel format.
--
-- The red, green, and blue components in the image are stored in 4-byte pixels in the order B, G, R from lowest to highest byte address within each pixel. The X component is ignored when compressing/encoding and undefined when decompressing/decoding.
pattern TJPF_BGRX :: TJPF
pattern TJPF_BGRX = TJPF #{const TJPF_BGRX}

-- | XBGR pixel format.
--
-- The red, green, and blue components in the image are stored in 4-byte pixels in the order R, G, B from highest to lowest byte address within each pixel. The X component is ignored when compressing/encoding and undefined when decompressing/decoding.
pattern TJPF_XBGR :: TJPF
pattern TJPF_XBGR = TJPF #{const TJPF_XBGR}

-- | XRGB pixel format.
--
-- The red, green, and blue components in the image are stored in 4-byte pixels in the order B, G, R from highest to lowest byte address within each pixel. The X component is ignored when compressing/encoding and undefined when decompressing/decoding.
pattern TJPF_XRGB :: TJPF
pattern TJPF_XRGB = TJPF #{const TJPF_XRGB}

-- | Grayscale pixel format.
--
-- Each 1-byte pixel represents a luminance (brightness) level from 0 to 255.
pattern TJPF_GRAY :: TJPF
pattern TJPF_GRAY = TJPF #{const TJPF_GRAY}

-- | RGBA pixel format.
--
-- This is the same as TJPF_RGBX, except that when decompressing/decoding, the X component is guaranteed to be 0xFF, which can be interpreted as an opaque alpha channel.
pattern TJPF_RGBA :: TJPF
pattern TJPF_RGBA = TJPF #{const TJPF_RGBA}

-- | BGRA pixel format.
--
-- This is the same as TJPF_BGRX, except that when decompressing/decoding, the X component is guaranteed to be 0xFF, which can be interpreted as an opaque alpha channel.
pattern TJPF_BGRA :: TJPF
pattern TJPF_BGRA = TJPF #{const TJPF_BGRA}

-- | ABGR pixel format.
--
-- This is the same as TJPF_XBGR, except that when decompressing/decoding, the X component is guaranteed to be 0xFF, which can be interpreted as an opaque alpha channel.
pattern TJPF_ABGR :: TJPF
pattern TJPF_ABGR = TJPF #{const TJPF_ABGR}

-- | ARGB pixel format.
--
-- This is the same as TJPF_XRGB, except that when decompressing/decoding, the X component is guaranteed to be 0xFF, which can be interpreted as an opaque alpha channel.
pattern TJPF_ARGB :: TJPF
pattern TJPF_ARGB = TJPF #{const TJPF_ARGB}

-- | CMYK pixel format.
--
-- Unlike RGB, which is an additive color model used primarily for display, CMYK (Cyan/Magenta/Yellow/Key) is a subtractive color model used primarily for printing. In the CMYK color model, the value of each color component typically corresponds to an amount of cyan, magenta, yellow, or black ink that is applied to a white background. In order to convert between CMYK and RGB, it is necessary to use a color management system (CMS.) A CMS will attempt to map colors within the printer's gamut to perceptually similar colors in the display's gamut and vice versa, but the mapping is typically not 1:1 or reversible, nor can it be defined with a simple formula. Thus, such a conversion is out of scope for a codec library. However, the TurboJPEG API allows for compressing packed-pixel CMYK images into YCCK JPEG images (see TJCS_YCCK) and decompressing YCCK JPEG images into packed-pixel CMYK images.
pattern TJPF_CMYK :: TJPF
pattern TJPF_CMYK = TJPF #{const TJPF_CMYK}

-- | Unknown pixel format.
--
-- Currently this is only used by tjLoadImage().
pattern TJPF_UNKNOWN :: TJPF
pattern TJPF_UNKNOWN = TJPF (#{const TJPF_UNKNOWN})

tjpfNames :: [(TJPF, String)]
tjpfNames =
  [ (TJPF_RGB, "TJPF_RGB")
  , (TJPF_BGR, "TJPF_BGR")
  , (TJPF_RGBX, "TJPF_RGBX")
  , (TJPF_BGRX, "TJPF_BGRX")
  , (TJPF_XBGR, "TJPF_XBGR")
  , (TJPF_XRGB, "TJPF_XRGB")
  , (TJPF_GRAY, "TJPF_GRAY")
  , (TJPF_RGBA, "TJPF_RGBA")
  , (TJPF_BGRA, "TJPF_BGRA")
  , (TJPF_ABGR, "TJPF_ABGR")
  , (TJPF_ARGB, "TJPF_ARGB")
  , (TJPF_CMYK, "TJPF_CMYK")
  , (TJPF_UNKNOWN, "TJPF_UNKNOWN")
  ]

tjPixelSize :: TJPF -> Maybe CInt
tjPixelSize = \case
  -- TJPF_RGB -> Just #{const tjPixelSize[TJPF_RGB]} -- BUG: some CCs throw "variable length array declaration cannot have 'static' storage duration"
  TJPF_RGB -> Just 3
  TJPF_BGR -> Just 3
  TJPF_RGBX -> Just 4
  TJPF_BGRX -> Just 4
  TJPF_XBGR -> Just 4
  TJPF_XRGB -> Just 4
  TJPF_GRAY -> Just 1
  TJPF_RGBA -> Just 4
  TJPF_BGRA -> Just 4
  TJPF_ABGR -> Just 4
  TJPF_ARGB -> Just 4
  TJPF_CMYK -> Just 4
  _ -> Nothing

-- ** Flags

newtype TJFLAG = TJFLAG CInt
  deriving (Eq, Ord, Show, Bits, Storable)

instance Semigroup TJFLAG where
 TJFLAG a <> TJFLAG b = TJFLAG (a .|. b)

instance Monoid TJFLAG where
  mempty = TJFLAG 0

-- | Use the most accurate DCT/IDCT algorithm available.
--
-- The default if this flag is not specified is implementation-specific. For example, the implementation of the TurboJPEG API in libjpeg-turbo uses the fast algorithm by default when compressing, because this has been shown to have only a very slight effect on accuracy, but it uses the accurate algorithm when decompressing, because this has been shown to have a larger effect.
pattern TJFLAG_ACCURATEDCT :: TJFLAG
pattern TJFLAG_ACCURATEDCT = TJFLAG #{const TJFLAG_ACCURATEDCT}

-- | Rows in the packed-pixel source/destination image are stored in bottom-up (Windows, OpenGL) order rather than in top-down (X11) order.
pattern TJFLAG_BOTTOMUP :: TJFLAG
pattern TJFLAG_BOTTOMUP = TJFLAG #{const TJFLAG_BOTTOMUP}

-- | Use the fastest DCT/IDCT algorithm available.
--
-- The default if this flag is not specified is implementation-specific.
-- For example, the implementation of the TurboJPEG API in libjpeg-turbo uses the fast algorithm by default when compressing, because this has been shown to have only a very slight effect on accuracy, but it uses the accurate algorithm when decompressing, because this has been shown to have a larger effect.
pattern TJFLAG_FASTDCT :: TJFLAG
pattern TJFLAG_FASTDCT = TJFLAG #{const TJFLAG_FASTDCT}

-- | When decompressing an image that was generated using chrominance subsampling, use the fastest chrominance upsampling algorithm available.
--
-- The default is to use smooth upsampling, which creates a smooth transition between neighboring chrominance components in order to reduce upsampling artifacts in the decompressed image.
pattern TJFLAG_FASTUPSAMPLE :: TJFLAG
pattern TJFLAG_FASTUPSAMPLE = TJFLAG #{const TJFLAG_FASTUPSAMPLE}

-- | Limit the number of progressive JPEG scans that the decompression and transform functions will process.
--
-- If a progressive JPEG image contains an unreasonably large number of scans, then this flag will cause the decompression and transform functions to return an error.
-- The primary purpose of this is to allow security-critical applications to guard against an exploit of the progressive JPEG format described in <https://libjpeg-turbo.org/pmwiki/uploads/About/TwoIssueswiththeJPEGStandard.pdf this report>.
pattern TJFLAG_LIMITSCANS :: TJFLAG
pattern TJFLAG_LIMITSCANS = TJFLAG #{const TJFLAG_LIMITSCANS}

-- | Disable JPEG buffer (re)allocation.
--
-- If passed to one of the JPEG compression or transform functions, this flag will cause those functions to generate an error if the JPEG destination buffer is invalid or too small, rather than attempt to allocate or reallocate that buffer.
pattern TJFLAG_NOREALLOC :: TJFLAG
pattern TJFLAG_NOREALLOC = TJFLAG #{const TJFLAG_NOREALLOC}

-- | When compressing or transforming, generate a progressive JPEG image instead of a single-scan JPEG image.
--
-- Progressive JPEG images generally have better compression ratios than single-scan JPEG images (much better if the image has large areas of solid color), but progressive JPEG compression and decompression is considerably slower than single-scan JPEG compression and decompression.
pattern TJFLAG_PROGRESSIVE :: TJFLAG
pattern TJFLAG_PROGRESSIVE = TJFLAG #{const TJFLAG_PROGRESSIVE}

-- | Immediately discontinue the current compression/decompression/transform operation if a warning (non-fatal error) occurs.
--
-- The default behavior is to allow the operation to complete unless a fatal error is encountered.
pattern TJFLAG_STOPONWARNING :: TJFLAG
pattern TJFLAG_STOPONWARNING = TJFLAG #{const TJFLAG_STOPONWARNING}

tjflagNames :: [(TJFLAG, String)]
tjflagNames =
  [ (TJFLAG_ACCURATEDCT, "TJFLAG_ACCURATEDCT")
  , (TJFLAG_BOTTOMUP, "TJFLAG_BOTTOMUP")
  , (TJFLAG_FASTDCT, "TJFLAG_FASTDCT")
  , (TJFLAG_FASTUPSAMPLE, "TJFLAG_FASTUPSAMPLE")
  , (TJFLAG_LIMITSCANS, "TJFLAG_LIMITSCANS")
  , (TJFLAG_NOREALLOC, "TJFLAG_NOREALLOC")
  , (TJFLAG_PROGRESSIVE, "TJFLAG_PROGRESSIVE")
  , (TJFLAG_STOPONWARNING, "TJFLAG_STOPONWARNING")
  ]

-- ** Transform options

newtype TJXOPT = TJXOPT CInt
  deriving (Eq, Ord, Show, Bits, Storable)

-- | Do not copy any extra markers (including Exif and ICC profile data) from the source image to the destination image.
pattern TJXOPT_COPYNONE :: TJXOPT
pattern TJXOPT_COPYNONE = TJXOPT #{const TJXOPT_COPYNONE}

-- | Enable lossless cropping.
--
-- See tjTransform() for more information.
pattern TJXOPT_CROP :: TJXOPT
pattern TJXOPT_CROP = TJXOPT #{const TJXOPT_CROP}

-- | Discard the color data in the source image, and generate a grayscale destination image.
pattern TJXOPT_GRAY :: TJXOPT
pattern TJXOPT_GRAY = TJXOPT #{const TJXOPT_GRAY}

-- | Do not generate a destination image.
--
-- (This can be used in conjunction with a custom filter to capture the transformed DCT coefficients without transcoding them.)
pattern TJXOPT_NOOUTPUT :: TJXOPT
pattern TJXOPT_NOOUTPUT = TJXOPT #{const TJXOPT_NOOUTPUT}

-- | This option causes tjTransform() to return an error if the transform is not perfect.
--
-- Lossless transforms operate on iMCUs, the size of which depends on the level of chrominance subsampling used (see tjMCUWidth and tjMCUHeight.)
-- If the image's width or height is not evenly divisible by the iMCU size, then there will be partial iMCUs on the right and/or bottom edges.
-- It is not possible to move these partial iMCUs to the top or left of the image, so any transform that would require that is "imperfect."
-- If this option is not specified, then any partial iMCUs that cannot be transformed will be left in place, which will create odd-looking strips on the right or bottom edge of the image.
pattern TJXOPT_PERFECT :: TJXOPT
pattern TJXOPT_PERFECT = TJXOPT #{const TJXOPT_PERFECT}

-- | Generate a progressive destination image instead of a single-scan destination image.
--
-- Progressive JPEG images generally have better compression ratios than single-scan JPEG images (much better if the image has large areas of solid color),
-- but progressive JPEG decompression is considerably slower than single-scan JPEG decompression.
pattern TJXOPT_PROGRESSIVE :: TJXOPT
pattern TJXOPT_PROGRESSIVE = TJXOPT #{const TJXOPT_PROGRESSIVE}

-- | Discard any partial iMCUs that cannot be transformed.
pattern TJXOPT_TRIM :: TJXOPT
pattern TJXOPT_TRIM = TJXOPT #{const TJXOPT_TRIM}

tjxoptNames :: [(TJXOPT, String)]
tjxoptNames =
  [ (TJXOPT_COPYNONE, "TJXOPT_COPYNONE")
  , (TJXOPT_CROP, "TJXOPT_CROP")
  , (TJXOPT_GRAY, "TJXOPT_GRAY")
  , (TJXOPT_NOOUTPUT, "TJXOPT_NOOUTPUT")
  , (TJXOPT_PERFECT, "TJXOPT_PERFECT")
  , (TJXOPT_PROGRESSIVE, "TJXOPT_PROGRESSIVE")
  , (TJXOPT_TRIM, "TJXOPT_TRIM")
  ]

-- ** Transform operations for 'tjTransform'

newtype TJXOP = TJXOP CInt
  deriving (Eq, Ord, Show, Bits, Storable)

-- | Do not transform the position of the image pixels.
pattern TJXOP_NONE :: TJXOP
pattern TJXOP_NONE = TJXOP #{const TJXOP_NONE}

-- | Flip (mirror) image horizontally.
--
-- This transform is imperfect if there are any partial iMCUs on the right edge (see TJXOPT_PERFECT.)
pattern TJXOP_HFLIP :: TJXOP
pattern TJXOP_HFLIP = TJXOP #{const TJXOP_HFLIP}

-- | Flip (mirror) image vertically.
--
-- This transform is imperfect if there are any partial iMCUs on the bottom edge (see TJXOPT_PERFECT.)
pattern TJXOP_VFLIP :: TJXOP
pattern TJXOP_VFLIP = TJXOP #{const TJXOP_VFLIP}

-- | Transpose image (flip/mirror along upper left to lower right axis.) This transform is always perfect.
pattern TJXOP_TRANSPOSE :: TJXOP
pattern TJXOP_TRANSPOSE = TJXOP #{const TJXOP_TRANSPOSE}

-- | Transverse transpose image (flip/mirror along upper right to lower left axis.) This transform is imperfect if there are any partial iMCUs in the image (see TJXOPT_PERFECT.)
pattern TJXOP_TRANSVERSE :: TJXOP
pattern TJXOP_TRANSVERSE = TJXOP #{const TJXOP_TRANSVERSE}

-- | Rotate image clockwise by 90 degrees.
--
-- This transform is imperfect if there are any partial iMCUs on the bottom edge (see TJXOPT_PERFECT.)
pattern TJXOP_ROT90 :: TJXOP
pattern TJXOP_ROT90 = TJXOP #{const TJXOP_ROT90}

-- | Rotate image 180 degrees.
--
-- This transform is imperfect if there are any partial iMCUs in the image (see TJXOPT_PERFECT.)
pattern TJXOP_ROT180 :: TJXOP
pattern TJXOP_ROT180 = TJXOP #{const TJXOP_ROT180}

-- | Rotate image counter-clockwise by 90 degrees.
--
-- This transform is imperfect if there are any partial iMCUs on the right edge (see TJXOPT_PERFECT.)
pattern TJXOP_ROT270 :: TJXOP
pattern TJXOP_ROT270 = TJXOP #{const TJXOP_ROT270}

tjxopNames :: [(TJXOP, String)]
tjxopNames =
  [ (TJXOP_NONE, "TJXOP_NONE")
  , (TJXOP_HFLIP, "TJXOP_HFLIP")
  , (TJXOP_VFLIP, "TJXOP_VFLIP")
  , (TJXOP_TRANSPOSE, "TJXOP_TRANSPOSE")
  , (TJXOP_TRANSVERSE, "TJXOP_TRANSVERSE")
  , (TJXOP_ROT90, "TJXOP_ROT90")
  , (TJXOP_ROT180, "TJXOP_ROT180")
  , (TJXOP_ROT270, "TJXOP_ROT270")
  ]

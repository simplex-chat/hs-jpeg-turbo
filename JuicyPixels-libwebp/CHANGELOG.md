# Changelog for `JuicyPixels-libwebp`

## Unreleased

- WebP decoding of JuicyPixels images with libwebp at commit 68dd1c30, the commit of Chrome 158 and 159: still images, the first frame of animations and alpha; the payloads of the ICCP and EXIF chunks.
- The image is decoded at the size that outputSize returns for the canvas size, scaled by libwebp in the same pass; each side at most the canvas side and at least 1/256 of it.
- decodeWebp refuses a canvas with a side above 16384 pixels; the allocations of libwebp's demuxer and decoder and the output image are counted together, and an allocation that would exceed maxMemoryMB fails, so that the image is refused; the demuxer and the decoder of different calls run one at a time.
- libwebp is compiled with NDEBUG, as in Chrome's release builds.

## 0.1.0.0 - YYYY-MM-DD

# Changelog for `jpeg-turbo`

## Unreleased

- cbits/jconfig.h, cbits/jconfigint.h, cbits/jversion.h and cbits/neon-compat.h are modified from mozjpeg 08265790 jconfig.h.in, jconfigint.h.in, jversion.h.in and simd/arm/neon-compat.h.in: the CMake substitutions are replaced by the values CMake computes, with compiler-macro tests for the target-dependent ones. THREAD_LOCAL is empty on 32-bit Android Arm, as in CMake builds without thread-local storage.
- The pkgconfig, static and static-gcc flags are removed; mozjpeg is compiled from the bundled source.
- The TurboJPEG3 and MozJPEG modules are added. TurboJPEG2: tjGetScalingFactors, TjScalingFactor and TJFLAG_LIMITSCANS are added; tjGetErrorStr2 and tjGetErrorCode are IO actions; tjGetErrorCode returns CInt and tjBufSize returns CULong.

## 0.1.0.0 - YYYY-MM-DD

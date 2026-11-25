#!/usr/bin/env bash

set -euo pipefail

builddir=dist-newstyle/coverage
summary=${GITHUB_STEP_SUMMARY:-/dev/null}

report() {
  {
    echo "### $1"
    echo '```'
    "${@:2}"
    echo '```'
  } | tee -a "$summary"
}

encode_coverage() {
  gcov -b -n "$(find "$builddir" -name encode.o.gcda)" | grep -A4 "^File 'cbits/encode.c'"
}

libwebp_coverage() {
  for object in vp8l_dec io_dec alpha_dec hs_webp; do
    gcov -f -n "$(find "$builddir" -name "$object.o.gcda")"
  done | grep -A1 -E "^Function '(AllocateAndInitRescaler|EmitRescaledRowsRGBA|InitRGBRescaler|EmitRescaledRGB|EmitRescaledAlphaRGB|ShiftWindow8b|ShiftWindow32b|VP8LShiftWindowBuffer|WebPShiftAlphaWindow|allocate|release|HsWebPDemux|HsWebPDecode)'"
}

rm -f jpeg-turbo/*.tix JuicyPixels-jpeg-turbo/*.tix JuicyPixels-spng/*.tix JuicyPixels-libwebp/*.tix
cabal test all --builddir="$builddir" --ghc-options=-fhpc --ghc-options=-optc--coverage --ghc-options=-optl--coverage
hpc sum --union --output="$builddir/jpeg-turbo.tix" --include=TurboJPEG2 --include=TurboJPEG3 --include=MozJPEG jpeg-turbo/jpeg-turbo-test.tix JuicyPixels-jpeg-turbo/JuicyPixels-jpeg-turbo-test.tix
report jpeg-turbo hpc report "$builddir/jpeg-turbo.tix" --hpcdir=jpeg-turbo/.hpc
report JuicyPixels-jpeg-turbo hpc report JuicyPixels-jpeg-turbo/JuicyPixels-jpeg-turbo-test.tix --hpcdir=JuicyPixels-jpeg-turbo/.hpc --include=Codec.Picture.JPEGTurbo
report JuicyPixels-spng hpc report JuicyPixels-spng/JuicyPixels-spng-test.tix --hpcdir=JuicyPixels-spng/.hpc --include=Codec.Picture.Spng
report JuicyPixels-libwebp hpc report JuicyPixels-libwebp/JuicyPixels-libwebp-test.tix --hpcdir=JuicyPixels-libwebp/.hpc --include=Codec.Picture.Webp
report jpeg-turbo/cbits/encode.c encode_coverage
report "libwebp scaling, sliding windows and memory count, JuicyPixels-libwebp tests" libwebp_coverage

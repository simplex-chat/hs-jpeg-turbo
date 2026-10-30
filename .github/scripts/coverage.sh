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

rm -f jpeg-turbo/*.tix JuicyPixels-jpeg-turbo/*.tix JuicyPixels-spng/*.tix
cabal test all --builddir="$builddir" --ghc-options=-fhpc --ghc-options=-optc--coverage --ghc-options=-optl--coverage
hpc sum --union --output="$builddir/jpeg-turbo.tix" --include=TurboJPEG2 --include=TurboJPEG3 --include=MozJPEG jpeg-turbo/jpeg-turbo-test.tix JuicyPixels-jpeg-turbo/JuicyPixels-jpeg-turbo-test.tix
report jpeg-turbo hpc report "$builddir/jpeg-turbo.tix" --hpcdir=jpeg-turbo/.hpc
report JuicyPixels-jpeg-turbo hpc report JuicyPixels-jpeg-turbo/JuicyPixels-jpeg-turbo-test.tix --hpcdir=JuicyPixels-jpeg-turbo/.hpc --include=Codec.Picture.JPEGTurbo
report JuicyPixels-spng hpc report JuicyPixels-spng/JuicyPixels-spng-test.tix --hpcdir=JuicyPixels-spng/.hpc --include=Codec.Picture.Spng
report jpeg-turbo/cbits/encode.c encode_coverage

#!/usr/bin/env bash

set -euo pipefail

out=dist-newstyle/output-check

encode() {
  mkdir -p "$out/$1"
  "${CC:-cc}" -O2 -I jpeg-turbo/cbits/mozjpeg .github/scripts/encode.c "$2" ${LDFLAGS:-} -lm -o "$out/encode-$1.exe"
  "$out/encode-$1.exe" "$out/$1"
}

rm -rf "$out"
encode cabal "$(find dist-newstyle -name 'libHSjpeg-turbo-*.a' -not -name '*_p.a')"
encode cmake "$MOZJPEG_PREFIX/lib/libturbojpeg.a"
ls -l "$out/cabal"
diff -r "$out/cabal" "$out/cmake"
echo "$(ls "$out/cabal" | wc -l) JPEGs and decoded images are identical"

#!/usr/bin/env bash

set -euo pipefail

out=dist-newstyle/armv7-check
cc=arm-linux-gnueabihf-gcc
flags="-march=armv7-a -mfpu=neon -mfloat-abi=hard"

rm -rf "$out"
mkdir -p "$out/neon" "$out/c"
sources=$(sed -n '/^library/,/^test-suite/p' jpeg-turbo/jpeg-turbo.cabal | grep -oE 'cbits/[^ ]+\.c' | grep -v /aarch64/ | sort -u | sed 's|^|jpeg-turbo/|')
$cc $flags -O2 -static -DBMP_SUPPORTED -DPPM_SUPPORTED -DNEON_INTRINSICS -I jpeg-turbo/cbits -I jpeg-turbo/cbits/mozjpeg $sources .github/scripts/encode.c -lm -o "$out/encode.exe"
file "$out/encode.exe"
nm "$out/encode.exe" | grep -E ' T jsimd_.*_neon$'
"$out/encode.exe" "$out/neon"
JSIMD_FORCENONE=1 "$out/encode.exe" "$out/c"
diff -r "$out/neon" "$out/c"
echo "$(ls "$out/neon" | wc -l) JPEGs and decoded images are identical with Neon and with the C code paths"

#!/usr/bin/env bash

set -euo pipefail

out=dist-newstyle/armv7-check
cc=arm-linux-gnueabihf-gcc
flags="-march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -O2 -DNEON_INTRINSICS -I jpeg-turbo/cbits -I jpeg-turbo/cbits/mozjpeg"

archive() {
  mkdir -p "$out/$1"
  for source in $3; do
    $cc $flags $2 -c "$source" -o "$out/$1/$(basename "$source" .c).o"
  done
  arm-linux-gnueabihf-ar rcs "$out/lib$1.a" "$out/$1"/*.o
}

rm -rf "$out"
mkdir -p "$out/neon" "$out/c"
sources=$(sed -n '/^library$/,/^$/p' jpeg-turbo/jpeg-turbo.cabal | grep -oE 'cbits/[^ ]+\.c' | grep -v -e /aarch64/ -e -neon.c | sed 's|^|jpeg-turbo/|')
neon_sources=$(sed -n '/^library neon$/,/^$/p' jpeg-turbo/jpeg-turbo.cabal | grep -oE 'cbits/[^ ]+\.c' | sed 's|^|jpeg-turbo/|')
archive jpeg-turbo "-DBMP_SUPPORTED -DPPM_SUPPORTED" "$sources"
archive jpeg-turbo-neon -mfpu=neon "$neon_sources"
for program in encode simd; do
  $cc $flags -static ".github/scripts/$program.c" "$out/libjpeg-turbo.a" "$out/libjpeg-turbo-neon.a" -lm -o "$out/$program.exe"
done
file "$out/encode.exe"
nm "$out/encode.exe" | grep -E ' T jsimd_.*_neon$'
plain=$("$out/simd.exe")
linux32=$(setarch linux32 "$out/simd.exe")
echo "SIMD selected: $plain without the PER_LINUX32 personality, $linux32 with it"
[ "$plain" = C ]
[ "$linux32" = Neon ]
setarch linux32 "$out/encode.exe" "$out/neon"
"$out/encode.exe" "$out/c"
diff -r "$out/neon" "$out/c"
echo "$(ls "$out/neon" | wc -l) JPEGs and decoded images are identical with Neon and with the C code paths"

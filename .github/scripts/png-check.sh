#!/usr/bin/env bash

set -euo pipefail

out=dist-newstyle/png-check
package=JuicyPixels-spng
sources=$(sed -n '/^library$/,/^$/p' $package/$package.cabal | grep -oE 'cbits/[^ ]+\.c' | sed "s|^|$package/|")
options=$(sed -n '/^library$/,/^$/s/^  cc-options: //p' $package/$package.cabal)
flags="-O2 $options -I $package/cbits -I $package/cbits/libspng/spng -I $package/cbits/miniz"

rm -rf "$out"
mkdir -p "$out/armv7" "$out/aarch64"
arm-linux-gnueabihf-gcc -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -static $flags $sources .github/scripts/png.c -lm -o "$out/armv7.exe"
gcc $flags $sources .github/scripts/png.c -lm -o "$out/aarch64.exe"
file "$out/armv7.exe" "$out/aarch64.exe"
"$out/armv7.exe" "$out/armv7" $package/cbits/libspng/tests/images/*.png
"$out/aarch64.exe" "$out/aarch64" $package/cbits/libspng/tests/images/*.png
diff -r "$out/armv7" "$out/aarch64"
echo "$(ls "$out/armv7" | wc -l) PNGs decoded identically on armv7 and aarch64"

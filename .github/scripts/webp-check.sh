#!/usr/bin/env bash

set -euo pipefail

out=dist-newstyle/webp-check
package=JuicyPixels-libwebp
sources=$(sed -n '/^library$/,/^$/p' $package/$package.cabal | grep -oE 'cbits/[^ ]+\.c' | sed "s|^|$package/|")
options=$(sed -n '/^library$/,/^$/s/^  cc-options: //p' $package/$package.cabal)
flags="-O2 $options -I $package/cbits -I $package/cbits/libwebp"
armv7="-march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard"
md5s="$WEBP_TEST_DATA/libwebp_tests.md5"
files=$(sed -n 's|^[0-9a-f]*  \(.*\.webp\)\.pam$|'"$WEBP_TEST_DATA"'/\1|p' "$md5s")
tall="$WEBP_TEST_DATA/tall-lossless.webp $WEBP_TEST_DATA/tall-lossy-alpha.webp"

objects() {
  mkdir -p "$out/$1"
  for source in $sources; do
    $2 $flags $3 -c "$source" -o "$out/$1/$(basename "$source" .c).o"
  done
}

rm -rf "$out"
mkdir -p "$out/armv7" "$out/aarch64"
arm-linux-gnueabihf-gcc $armv7 -static $flags $sources .github/scripts/webp.c -lpthread -lm -o "$out/armv7.exe"
gcc $flags $sources .github/scripts/webp.c -lpthread -lm -o "$out/aarch64.exe"
clang-18 -g -fsanitize=address,undefined -fno-sanitize-recover=undefined $flags $sources .github/scripts/webp.c -lpthread -lm -o "$out/sanitized.exe"
file "$out/armv7.exe" "$out/aarch64.exe"

"$out/armv7.exe" check "$out/armv7" 16384 $files
"$out/aarch64.exe" check "$out/aarch64" 16384 $files
diff -r "$out/armv7" "$out/aarch64"
ls "$out/aarch64" > "$out/outputs.txt"
awk 'NR == FNR {outputs[$1]; next} $2 in outputs' "$out/outputs.txt" "$md5s" > "$out/expected.md5"
[ "$(wc -l < "$out/outputs.txt")" -eq 96 ]
[ "$(wc -l < "$out/expected.md5")" -eq 96 ]
(cd "$out/aarch64" && md5sum --check --quiet ../expected.md5)
echo "$(wc -l < "$out/outputs.txt") WebP files decoded identically on armv7 and aarch64, equal to libwebp_tests.md5"

for build in armv7 aarch64 sanitized; do
  mkdir -p "$out/tall-$build" "$out/scaled-$build"
  "$out/$build.exe" check "$out/tall-$build" 16384 $tall
  "$out/$build.exe" check "$out/scaled-$build" 64 $files $tall
done
diff -r "$out/tall-armv7" "$out/tall-aarch64"
diff -r "$out/tall-aarch64" "$out/tall-sanitized"
cmp "$out/tall-aarch64/tall-lossless.webp.pam" "$WEBP_TEST_DATA/tall-lossless.pam"
echo "$(ls "$out/tall-aarch64" | wc -l) tall WebP files decoded identically on armv7, aarch64 and with sanitizers, the lossless one equal to its source pixels"

diff -r "$out/scaled-aarch64" "$out/scaled-sanitized"
python3 -I - "$out/scaled-armv7" "$out/scaled-aarch64" <<'PY'
import os
import sys

def samples(path):
    data = open(path, "rb").read()
    if data.startswith(b"P7"):
        end = data.index(b"ENDHDR\n") + 7
        return 4, data[:end], data[end:]
    if data.startswith(b"P6"):
        end = len(b"\n".join(data.split(b"\n", 3)[:3])) + 1
        return 3, data[:end], data[end:]
    return 0, data, b""

c_dir, neon_dir = sys.argv[1:]
names = sorted(os.listdir(neon_dir))
differing, largest = 0, 0
for name in names:
    depth, c_header, c = samples(os.path.join(c_dir, name))
    _, neon_header, neon = samples(os.path.join(neon_dir, name))
    if c_header != neon_header:
        sys.exit(f"{name}: the armv7 and aarch64 outputs differ in their headers")
    if c == neon:
        continue
    differing += 1
    for i in range(0, len(c), depth):
        if depth == 4:
            a, b = c[i + 3], neon[i + 3]
            largest = max([largest, abs(a - b)] + [abs(c[i + k] * a - neon[i + k] * b) / 255 for k in range(3)])
        else:
            largest = max([largest] + [abs(c[i + k] - neon[i + k]) for k in range(3)])
print(f"{len(names)} WebP files decoded with sides of at most 64 identically on aarch64 and with sanitizers; armv7 and aarch64 outputs of {differing} files differ by at most {largest:.2f} in a sample, colour premultiplied by alpha")
sys.exit(1 if largest > 5 else 0)
PY

for side in 16384 64; do
  "$out/sanitized.exe" fuzz $side $files $tall
done

{
  echo "### Counted memory of the tall WebP files, bytes, within 256 MiB"
  echo '```'
  for build in armv7 aarch64; do
    for side in 16384 2048 64; do
      echo "$build, sides at most $side"
      "$out/$build.exe" memory 256 $side $tall
    done
  done
  echo '```'
} | tee -a "${GITHUB_STEP_SUMMARY:-/dev/null}"

objects armv7-objects arm-linux-gnueabihf-gcc "$armv7"
objects aarch64-objects gcc ""
{
  echo "### WebP decoder objects, $(echo "$sources" | wc -w) files"
  echo '```'
  echo armv7
  arm-linux-gnueabihf-size -t "$out/armv7-objects"/*.o | sed -n '1p;$p'
  echo aarch64
  size -t "$out/aarch64-objects"/*.o | sed -n '1p;$p'
  echo '```'
} | tee -a "${GITHUB_STEP_SUMMARY:-/dev/null}"

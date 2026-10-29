# Changelog for `JuicyPixels-spng`

## Unreleased

- PNG decoding and encoding of JuicyPixels images with libspng 0.7.4 and miniz 3.1.2.
- decodePng returns the ICC profile of the PNG, and encodePng embeds the ICC profile given in EncodeOptions.
- decodePng refuses a PNG with an iCCP chunk that decompresses to more than 1 MiB, or a zTXt or iTXt chunk that decompresses to more than 512 KiB.

## 0.1.0.0 - YYYY-MM-DD

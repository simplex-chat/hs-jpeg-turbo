# Changelog for `JuicyPixels-jpeg-turbo`

## Unreleased

- readJpegHeader and decodeJpeg are added, with the JpegHeader, Scale and DecodeOptions types.
- encodeRGB takes an optional ICC profile and embeds it in the JPEG.
- encodeRGB raises `error` for a side outside 1..65500, a data length that does not match the dimensions, a quality outside 0..100 and an ICC profile length outside 1..16707345.
- encodeRGB encodes with mozjpeg's default profile, so its output differs from the earlier TurboJPEG progressive 4:2:0 output.
- The library depends on transformers.

## 0.1.0.0 - YYYY-MM-DD

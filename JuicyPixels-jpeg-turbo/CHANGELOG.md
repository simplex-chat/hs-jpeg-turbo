# Changelog for `JuicyPixels-jpeg-turbo`

## Unreleased

- readJpegHeader and decodeJpeg are added, with the JpegHeader, Scale and DecodeOptions types.
- encodeRGB raises `error` for a side outside 1..65500, a data length that does not match the dimensions and a quality outside 0..100.
- encodeRGB encodes with mozjpeg's default profile, so its output differs from the earlier TurboJPEG progressive 4:2:0 output.
- The library depends on transformers.

## 0.1.0.0 - YYYY-MM-DD

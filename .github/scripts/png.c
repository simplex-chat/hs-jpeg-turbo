#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "spng.h"

static int decode(const unsigned char *png, size_t size, unsigned char **image, size_t *len)
{
  spng_ctx *ctx = spng_ctx_new(0);
  int rc = spng_set_png_buffer(ctx, png, size);

  if (!rc)
    rc = spng_set_image_limits(ctx, 16384, 16384);
  if (!rc)
    rc = spng_set_chunk_limits(ctx, 64 << 20, 64 << 20);
  if (!rc)
    rc = spng_decoded_image_size(ctx, SPNG_FMT_RGBA8, len);
  if (!rc && !(*image = malloc(*len)))
    rc = SPNG_EMEM;
  if (!rc)
    rc = spng_decode_image(ctx, *image, *len, SPNG_FMT_RGBA8, SPNG_DECODE_TRNS);
  spng_ctx_free(ctx);
  return rc;
}

static int convert(const char *input, const char *output)
{
  FILE *in = fopen(input, "rb"), *out;
  unsigned char *png, *image = NULL;
  long size;
  size_t len = 0;
  int rc;

  if (!in || fseek(in, 0, SEEK_END) != 0 || (size = ftell(in)) < 0 || fseek(in, 0, SEEK_SET) != 0 ||
      !(png = malloc(size + 1)) || fread(png, 1, size, in) != (size_t)size || fclose(in) != 0) {
    perror(input);
    return 1;
  }
  rc = decode(png, size, &image, &len);
  if (!(out = fopen(output, "wb")) ||
      (rc ? fprintf(out, "%s\n", spng_strerror(rc)) < 0 : fwrite(image, 1, len, out) != len) ||
      fclose(out) != 0) {
    perror(output);
    return 1;
  }
  free(png);
  free(image);
  return 0;
}

int main(int argc, char **argv)
{
  int i;

  if (argc < 3) {
    fprintf(stderr, "usage: %s DIRECTORY PNG...\n", argv[0]);
    return 2;
  }
  for (i = 2; i < argc; i++) {
    const char *name = strrchr(argv[i], '/') ? strrchr(argv[i], '/') + 1 : argv[i];
    char path[4096];

    snprintf(path, sizeof(path), "%s/%s.rgba", argv[1], name);
    if (convert(argv[i], path))
      return 1;
  }
  return 0;
}

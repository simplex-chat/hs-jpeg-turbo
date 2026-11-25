#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "src/webp/encode.h"

#define WIDTH 1024
#define HEIGHT 4096
#define PERIOD 960

static uint32_t hash(uint32_t x, uint32_t y)
{
  uint32_t h = x * 0x9E3779B1u ^ y * 0x85EBCA77u;
  h ^= h >> 15;
  h *= 0x2C1B3C6Du;
  return h ^ (h >> 12);
}

static void write_file(const char *dir, const char *name, const char *header, const uint8_t *data, size_t size)
{
  char path[4096];
  FILE *out;

  snprintf(path, sizeof(path), "%s/%s", dir, name);
  if (!(out = fopen(path, "wb")) || fputs(header, out) < 0 || fwrite(data, 1, size, out) != size || fclose(out) != 0) {
    perror(path);
    exit(1);
  }
}

static void encode(const char *dir, const char *name, const uint8_t *rgba, int lossless)
{
  WebPConfig config;
  WebPPicture picture;
  WebPMemoryWriter writer;
  WebPAuxStats stats;

  if (!WebPConfigInit(&config) || !WebPPictureInit(&picture)) {
    fprintf(stderr, "%s: libwebp version mismatch\n", name);
    exit(1);
  }
  config.lossless = lossless;
  config.exact = 1;
  config.quality = lossless ? 80 : 75;
  config.method = lossless ? 4 : 0;
  config.alpha_filtering = 0;
  picture.use_argb = lossless;
  picture.width = WIDTH;
  picture.height = HEIGHT;
  picture.writer = WebPMemoryWrite;
  picture.custom_ptr = &writer;
  picture.stats = &stats;
  WebPMemoryWriterInit(&writer);
  if (!WebPPictureImportRGBA(&picture, rgba, WIDTH * 4) || !WebPEncode(&config, &picture)) {
    fprintf(stderr, "%s: encoding error %d\n", name, picture.error_code);
    exit(1);
  }
  write_file(dir, name, "", writer.mem, writer.size);
  printf("%s: %dx%d, %lu bytes; %s stream: transforms 0x%x, palette %d, color cache bits %d\n", name, WIDTH, HEIGHT, (unsigned long)writer.size,
         lossless ? "lossless" : "alpha", stats.lossless_features, stats.palette_size, stats.cache_bits);
  WebPMemoryWriterClear(&writer);
  WebPPictureFree(&picture);
}

int main(int argc, char **argv)
{
  const size_t pixels = (size_t)WIDTH * HEIGHT;
  uint8_t *rgba = malloc(pixels * 4), *alpha = malloc(pixels), *pixel = rgba;
  uint32_t x, y, row, noise;
  char header[128];

  if (argc != 2 || !rgba || !alpha) {
    fprintf(stderr, "usage: %s DIRECTORY\n", argv[0]);
    return 2;
  }
  for (y = 0; y < HEIGHT; y++) {
    row = y % PERIOD;
    for (x = 0; x < WIDTH; x++, pixel += 4) {
      noise = hash(x, row);
      pixel[0] = (uint8_t)(x < WIDTH / 2 ? noise : x);
      pixel[1] = (uint8_t)(x < WIDTH / 2 ? noise >> 8 : row);
      pixel[2] = (uint8_t)(x < WIDTH / 2 ? noise >> 16 : (x ^ row) >> 2);
      pixel[3] = (uint8_t)(8 * (hash(x / 16, row / 16) % 32) + 7);
      alpha[y * WIDTH + x] = pixel[3];
    }
  }
  encode(argv[1], "tall-lossless.webp", rgba, 1);
  snprintf(header, sizeof(header), "P7\nWIDTH %d\nHEIGHT %d\nDEPTH 4\nMAXVAL 255\nTUPLTYPE RGB_ALPHA\nENDHDR\n", WIDTH, HEIGHT);
  write_file(argv[1], "tall-lossless.pam", header, rgba, pixels * 4);
  encode(argv[1], "tall-lossy-alpha.webp", rgba, 0);
  snprintf(header, sizeof(header), "P5\n%d %d\n255\n", WIDTH, HEIGHT);
  write_file(argv[1], "tall-lossy-alpha.pgm", header, alpha, pixels);
  free(alpha);
  free(rgba);
  return 0;
}

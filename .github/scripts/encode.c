#include <stdio.h>
#include <stdlib.h>
#include <turbojpeg.h>

typedef unsigned char (*pixel_fn)(int x, int y, int c);

static unsigned hash(unsigned x, unsigned y, unsigned c)
{
  unsigned v = x * 374761393u + y * 668265263u + c * 2246822519u;
  v = (v ^ (v >> 13)) * 1274126177u;
  return v ^ (v >> 16);
}

static int noise(int x, int y, int c, int cell)
{
  int gx = x / cell, gy = y / cell, fx = x % cell, fy = y % cell;
  int top = (int)(hash(gx, gy, c) & 255) * (cell - fx) + (int)(hash(gx + 1, gy, c) & 255) * fx;
  int bottom = (int)(hash(gx, gy + 1, c) & 255) * (cell - fx) + (int)(hash(gx + 1, gy + 1, c) & 255) * fx;
  return (top * (cell - fy) + bottom * fy) / (cell * cell);
}

static unsigned char photo(int x, int y, int c)
{
  int v = (2 * noise(x, y, c, 256) + noise(x, y, c, 32) + noise(x, y, c, 4)) / 4 + (int)(hash(x, y, c) & 15) - 8;
  return (unsigned char)(v < 0 ? 0 : v > 255 ? 255 : v);
}

static unsigned char gradient(int x, int y, int c)
{
  int v[3] = { x * 7 + y * 3, x * y, x + y * 11 };
  return (unsigned char)v[c];
}

static unsigned char pattern(int x, int y, int c)
{
  int v[3] = { ((x / 8 + y / 8) & 1) ? 230 : 20, (x + y) % 64 < 32 ? 200 : 40, (x * x + y * y) / 1000 };
  return (unsigned char)v[c];
}

static const struct {
  const char *name;
  int width, height;
  pixel_fn pixel;
} images[] = {
  { "photo", 4000, 3000, photo },
  { "gradient", 37, 23, gradient },
  { "pattern", 1001, 999, pattern }
};

static const int qualities[] = { 20, 50, 85, 95, 100 };

int jpeg_turbo_encode(const unsigned char *pixels, int width, int height,
                      int pixelFormat, int quality, unsigned char **jpeg,
                      size_t *jpegSize, char *error);

typedef int (*encode_fn)(const unsigned char *rgb, int width, int height, int quality, unsigned char **jpeg, unsigned long *size, char *error);

static int encode_mozjpeg(const unsigned char *rgb, int width, int height, int quality, unsigned char **jpeg, unsigned long *size, char *error)
{
  size_t jpegSize = 0;
  int retval = jpeg_turbo_encode(rgb, width, height, TJPF_RGB, quality, jpeg, &jpegSize, error);

  *size = jpegSize;
  return retval;
}

static int encode_baseline(const unsigned char *rgb, int width, int height, int quality, unsigned char **jpeg, unsigned long *size, char *error)
{
  tjhandle handle = tjInitCompress();
  int retval = tjCompress2(handle, rgb, width, 0, height, TJPF_RGB, jpeg, size, TJSAMP_420, quality, TJFLAG_STOPONWARNING);

  if (retval != 0)
    snprintf(error, 200, "%s", tjGetErrorStr2(handle));
  tjDestroy(handle);
  return retval;
}

static const struct {
  const char *name;
  encode_fn encode;
} encoders[] = {
  { "mozjpeg", encode_mozjpeg },
  { "baseline", encode_baseline }
};

static const int denominators[] = { 1, 2, 4, 8 };

static int save(const char *path, const unsigned char *data, size_t size)
{
  FILE *f = fopen(path, "wb");

  if (!f || fwrite(data, 1, size, f) != size || fclose(f) != 0) {
    perror(path);
    return 1;
  }
  return 0;
}

static int decode(const char *base, const unsigned char *jpeg, unsigned long size)
{
  tjhandle handle = tjInitDecompress();
  int width, height, subsamp, colorspace;
  size_t d;

  if (!handle || tjDecompressHeader3(handle, jpeg, size, &width, &height, &subsamp, &colorspace) != 0) {
    fprintf(stderr, "%s: %s\n", base, tjGetErrorStr2(handle));
    return 1;
  }
  for (d = 0; d < sizeof(denominators) / sizeof(denominators[0]); d++) {
    int w = (width + denominators[d] - 1) / denominators[d], h = (height + denominators[d] - 1) / denominators[d];
    size_t rgbSize = (size_t)w * h * 3;
    unsigned char *rgb = malloc(rgbSize);
    char path[4096];

    if (!rgb) {
      perror("malloc");
      return 1;
    }
    if (tjDecompress2(handle, jpeg, size, rgb, w, 0, h, TJPF_RGB, TJFLAG_STOPONWARNING | TJFLAG_LIMITSCANS) != 0) {
      fprintf(stderr, "%s 1/%d: %s\n", base, denominators[d], tjGetErrorStr2(handle));
      return 1;
    }
    snprintf(path, sizeof(path), "%s-%d.rgb", base, denominators[d]);
    if (save(path, rgb, rgbSize))
      return 1;
    free(rgb);
  }
  tjDestroy(handle);
  return 0;
}

int main(int argc, char **argv)
{
  size_t i, q, e;

  if (argc != 2) {
    fprintf(stderr, "usage: %s DIRECTORY\n", argv[0]);
    return 2;
  }
  for (i = 0; i < sizeof(images) / sizeof(images[0]); i++) {
    int w = images[i].width, h = images[i].height, x, y, c;
    unsigned char *rgb = malloc((size_t)w * h * 3);

    if (!rgb) {
      perror("malloc");
      return 1;
    }
    for (y = 0; y < h; y++)
      for (x = 0; x < w; x++)
        for (c = 0; c < 3; c++)
          rgb[((size_t)y * w + x) * 3 + c] = images[i].pixel(x, y, c);
    for (q = 0; q < sizeof(qualities) / sizeof(qualities[0]); q++)
      for (e = 0; e < sizeof(encoders) / sizeof(encoders[0]); e++) {
        unsigned char *jpeg = NULL;
        unsigned long size = 0;
        char base[4096], path[4096], error[200];

        if (encoders[e].encode(rgb, w, h, qualities[q], &jpeg, &size, error) != 0) {
          fprintf(stderr, "%s q%d %s: %s\n", images[i].name, qualities[q], encoders[e].name, error);
          return 1;
        }
        snprintf(base, sizeof(base), "%s/%s-q%d-%s", argv[1], images[i].name, qualities[q], encoders[e].name);
        snprintf(path, sizeof(path), "%s.jpg", base);
        if (save(path, jpeg, size) || decode(base, jpeg, size))
          return 1;
        free(jpeg);
      }
    free(rgb);
  }
  return 0;
}

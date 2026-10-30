#include "turbojpeg.h"

int pixel_size(int pixelFormat)
{
  return tjPixelSize[pixelFormat];
}

int mcu_width(int subsamp)
{
  return tjMCUWidth[subsamp];
}

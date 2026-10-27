#include <stdio.h>
#include "jpeglib.h"

unsigned int icc_profile(const unsigned char *jpeg, unsigned long size, JOCTET **icc)
{
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  unsigned int len;

  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_decompress(&cinfo);
  jpeg_mem_src(&cinfo, jpeg, size);
  jpeg_save_markers(&cinfo, JPEG_APP0 + 2, 0xFFFF);
  jpeg_read_header(&cinfo, TRUE);
  jpeg_read_icc_profile(&cinfo, icc, &len);
  jpeg_destroy_decompress(&cinfo);
  return len;
}

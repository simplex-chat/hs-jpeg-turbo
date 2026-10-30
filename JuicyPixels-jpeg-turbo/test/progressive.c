#include <stdio.h>
#include "jpeglib.h"

unsigned long progressive_jpeg(unsigned char **jpeg)
{
  jpeg_scan_info scans[2] = { { 1, { 0 }, 0, 0, 0, 0 }, { 1, { 0 }, 1, 63, 0, 0 } };
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPLE row[8] = { 0 };
  JSAMPROW rows[8] = { row, row, row, row, row, row, row, row };
  unsigned long size = 0;

  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_compress(&cinfo);
  *jpeg = NULL;
  jpeg_mem_dest(&cinfo, jpeg, &size);
  cinfo.image_width = 8;
  cinfo.image_height = 8;
  cinfo.input_components = 1;
  cinfo.in_color_space = JCS_GRAYSCALE;
  jpeg_c_set_int_param(&cinfo, JINT_COMPRESS_PROFILE, JCP_FASTEST);
  jpeg_set_defaults(&cinfo);
  cinfo.scan_info = scans;
  cinfo.num_scans = 2;
  jpeg_start_compress(&cinfo, TRUE);
  jpeg_write_scanlines(&cinfo, rows, 8);
  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);
  return size;
}

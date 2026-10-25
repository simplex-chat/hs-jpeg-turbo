#include <stdio.h>
#include "jpeglib.h"

#define MAX_AL  10

unsigned long many_scans_jpeg(unsigned char **jpeg)
{
  jpeg_scan_info scans[DCTSIZE2 * (MAX_AL + 1)];
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPLE row[8] = { 0 };
  JSAMPROW rows[8] = { row, row, row, row, row, row, row, row };
  unsigned long size = 0;
  int n = 0, k, al;

  for (k = 0; k < DCTSIZE2; k++)
    for (al = MAX_AL; al >= 0; al--, n++) {
      scans[n].comps_in_scan = 1;
      scans[n].component_index[0] = 0;
      scans[n].Ss = scans[n].Se = k;
      scans[n].Ah = al == MAX_AL ? 0 : al + 1;
      scans[n].Al = al;
    }
  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_compress(&cinfo);
  *jpeg = NULL;
  jpeg_mem_dest(&cinfo, jpeg, &size);
  cinfo.image_width = 8;
  cinfo.image_height = 8;
  cinfo.input_components = 1;
  cinfo.in_color_space = JCS_GRAYSCALE;
  jpeg_set_defaults(&cinfo);
  cinfo.scan_info = scans;
  cinfo.num_scans = n;
  jpeg_start_compress(&cinfo, TRUE);
  jpeg_write_scanlines(&cinfo, rows, 8);
  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);
  return size;
}

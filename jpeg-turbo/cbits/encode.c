#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include "jpeglib.h"
#include "jerror.h"
#include "turbojpeg.h"

struct encode_error {
  struct jpeg_error_mgr pub;
  jmp_buf jump;
};

struct encode_destination {
  struct jpeg_destination_mgr pub;
  unsigned char *buffer;
  size_t size;
};

static const J_COLOR_SPACE pf2cs[TJ_NUMPF] = {
  JCS_EXT_RGB, JCS_EXT_BGR, JCS_EXT_RGBX, JCS_EXT_BGRX, JCS_EXT_XBGR,
  JCS_EXT_XRGB, JCS_GRAYSCALE, JCS_EXT_RGBA, JCS_EXT_BGRA, JCS_EXT_ABGR,
  JCS_EXT_ARGB, JCS_CMYK
};

static void error_exit(j_common_ptr cinfo)
{
  longjmp(((struct encode_error *)cinfo->err)->jump, 1);
}

static void emit_message(j_common_ptr cinfo, int msg_level)
{
  if (msg_level < 0)
    error_exit(cinfo);
}

static void init_destination(j_compress_ptr cinfo)
{
  struct encode_destination *dest = (struct encode_destination *)cinfo->dest;

  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = dest->size;
}

static boolean empty_output_buffer(j_compress_ptr cinfo)
{
  struct encode_destination *dest = (struct encode_destination *)cinfo->dest;
  unsigned char *buffer = realloc(dest->buffer, dest->size * 2);

  if (!buffer)
    ERREXIT1(cinfo, JERR_OUT_OF_MEMORY, 10);
  dest->pub.next_output_byte = buffer + dest->size;
  dest->pub.free_in_buffer = dest->size;
  dest->buffer = buffer;
  dest->size *= 2;
  return TRUE;
}

static void term_destination(j_compress_ptr cinfo)
{
}

int jpeg_turbo_encode(const unsigned char *pixels, int width, int height,
                      int pixelFormat, int quality, unsigned char **jpeg,
                      size_t *jpegSize, char *error)
{
  struct jpeg_compress_struct cinfo;
  struct encode_error jerr;
  struct encode_destination dest;
  JSAMPROW row;

  dest.size = 4096;
  if (!(dest.buffer = malloc(dest.size))) {
    snprintf(error, JMSG_LENGTH_MAX, "Memory allocation failure");
    return -1;
  }
  dest.pub.init_destination = init_destination;
  dest.pub.empty_output_buffer = empty_output_buffer;
  dest.pub.term_destination = term_destination;
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = error_exit;
  jerr.pub.emit_message = emit_message;
  if (setjmp(jerr.jump)) {
    (*cinfo.err->format_message) ((j_common_ptr)&cinfo, error);
    jpeg_destroy_compress(&cinfo);
    free(dest.buffer);
    return -1;
  }
  jpeg_create_compress(&cinfo);
  cinfo.dest = &dest.pub;
  cinfo.image_width = width;
  cinfo.image_height = height;
  cinfo.in_color_space = pf2cs[pixelFormat];
  cinfo.input_components = tjPixelSize[pixelFormat];
  jpeg_set_defaults(&cinfo);
  jpeg_set_quality(&cinfo, quality, TRUE);
  jpeg_start_compress(&cinfo, TRUE);
  while (cinfo.next_scanline < cinfo.image_height) {
    row = (JSAMPROW)&pixels[(size_t)cinfo.next_scanline * width * tjPixelSize[pixelFormat]];
    jpeg_write_scanlines(&cinfo, &row, 1);
  }
  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);
  *jpeg = dest.buffer;
  *jpegSize = dest.size - dest.pub.free_in_buffer;
  return 0;
}

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "hs_webp.h"
#include "src/utils/palette.h"
#include "src/utils/utils.h"
#include "src/webp/encode.h"

typedef union {
  struct {
    size_t size;
    HsWebPMemory *owner;
  } block;
  unsigned char bytes[16];
} Header;

static HsWebPMemory *active;

static void *allocate(size_t size, int zero)
{
  HsWebPMemory *memory = active;
  Header *header;

  if (!memory)
    return NULL;
  if (memory->used > memory->limit || memory->limit - memory->used < sizeof(Header) || size > memory->limit - memory->used - sizeof(Header)) {
    memory->refused = 1;
    return NULL;
  }
  header = zero ? calloc(1, size + sizeof(Header)) : malloc(size + sizeof(Header));
  if (!header)
    return NULL;
  header->block.size = size + sizeof(Header);
  header->block.owner = memory;
  memory->used += header->block.size;
  if (memory->used > memory->peak)
    memory->peak = memory->used;
  return header + 1;
}

static void release(void *ptr)
{
  Header *header;

  if (!ptr)
    return;
  header = (Header *)ptr - 1;
  header->block.owner->used -= header->block.size;
  free(header);
}

#define malloc(size) allocate(size, 0)
#define calloc(count, size) allocate((count) * (size), 1)
#define free(ptr) release(ptr)
#include "libwebp/src/utils/utils.c"
#undef malloc
#undef calloc
#undef free

WebPDemuxer *HsWebPDemux(const uint8_t *data, size_t size, HsWebPMemory *memory)
{
  WebPData webp;
  WebPDemuxer *demux;

  webp.bytes = data;
  webp.size = size;
  memory->used = 0;
  memory->peak = 0;
  memory->refused = 0;
  active = memory;
  demux = WebPDemux(&webp);
  active = NULL;
  return demux;
}

VP8StatusCode HsWebPDecode(const uint8_t *data, size_t size, WebPDecoderConfig *config, HsWebPMemory *memory)
{
  VP8StatusCode status;

  active = memory;
  status = WebPDecode(data, size, config);
  active = NULL;
  return status;
}

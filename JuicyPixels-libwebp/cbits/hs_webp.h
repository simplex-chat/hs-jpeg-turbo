#ifndef HS_WEBP_H
#define HS_WEBP_H

#include <stddef.h>
#include <stdint.h>
#include "src/webp/decode.h"
#include "src/webp/demux.h"

typedef struct {
  size_t limit;
  size_t used;
  size_t peak;
  int refused;
} HsWebPMemory;

WebPDemuxer *HsWebPDemux(const uint8_t *data, size_t size, HsWebPMemory *memory);

VP8StatusCode HsWebPDecode(const uint8_t *data, size_t size, WebPDecoderConfig *config, HsWebPMemory *memory);

#endif

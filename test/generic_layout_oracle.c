#include "generic_layout_oracle.h"

#include <stdint.h>

struct sized_u8_2 {
  uint8_t data[2];
};

struct sized_u16_2 {
  uint16_t data[2];
};

struct bytes_2 {
  uint8_t data[2];
};

struct holder {
  struct bytes_2 value;
};

struct layout_box_u16_2 {
  uint8_t lead;
  uint16_t payload[2];
};

struct layout_box_u64_2 {
  uint8_t lead;
  uint64_t payload[2];
};

struct layout_box_u16_1 {
  uint8_t lead;
  uint16_t payload[1];
};

struct layout_box_nested {
  uint8_t lead;
  struct layout_box_u16_1 payload[2];
};

struct envelope_u16_2 {
  uint8_t prefix;
  struct layout_box_u16_2 box;
  uint32_t tail;
};

size_t oracle_size_u8_2(void) { return sizeof(struct sized_u8_2); }
size_t oracle_size_u16_2(void) { return sizeof(struct sized_u16_2); }
size_t oracle_holder_size(void) { return sizeof(struct holder); }
size_t oracle_layout_box_size(void) { return sizeof(struct layout_box_u16_2); }
size_t oracle_layout_box_align(void) { return _Alignof(struct layout_box_u64_2); }
size_t oracle_payload_offset(void) {
  return offsetof(struct layout_box_u16_2, payload);
}
size_t oracle_nested_size(void) { return sizeof(struct layout_box_nested); }
size_t oracle_envelope_size(void) { return sizeof(struct envelope_u16_2); }
size_t oracle_envelope_box_offset(void) {
  return offsetof(struct envelope_u16_2, box);
}
size_t oracle_envelope_tail_offset(void) {
  return offsetof(struct envelope_u16_2, tail);
}

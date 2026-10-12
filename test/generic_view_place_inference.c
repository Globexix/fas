#include <stdint.h>

struct Packet {
  uint8_t prefix;
  uint8_t values[2];
};

uint32_t expected_field(void *pointer) {
  return ((struct Packet *)pointer)->prefix;
}

uint32_t expected_second(void *pointer) {
  return ((uint8_t *)pointer)[1];
}

uint32_t expected_packet_element(void *pointer) {
  return ((struct Packet *)pointer)->values[1];
}

uint32_t field_shadow_c(void) { return 1; }
uint32_t call_shadow_c(void) { return 1; }
uint32_t elem_shadow_c(void) { return 1; }

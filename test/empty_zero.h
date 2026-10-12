#include <stdint.h>
union FasZeroUnion { uint64_t word; uint8_t bytes[16]; };
static inline void fas_keep_bytes(void *pointer) {
  __asm__ volatile("" : : "r"(pointer) : "memory");
}

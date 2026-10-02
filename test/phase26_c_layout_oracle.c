#include "phase26_c_layout.h"

#include <stddef.h>

enum FasByteEnum fas_enum_values[4] = {
  FasByte0, FasByteMax, FasByte0, FasByteMax
};

struct FasPackedStorage fas_packed_storage = {
  7, UINT32_C(0x12345678), 3.5
};

void fas_seed_deep(struct FasDeepMin *value) {
  value->a = 9;
  value->b = 0x4567;
  value->x = UINT32_C(0x44556677);
  value->z = UINT64_C(0x1122334455667788);
}

uint32_t fas_read_deep_x(struct FasDeepMin *value) { return value->x; }
size_t fas_c_deep_size(void) { return sizeof(struct FasDeepMin); }
size_t fas_c_deep_align(void) { return _Alignof(struct FasDeepMin); }
size_t fas_c_deep_x_offset(void) { return offsetof(struct FasDeepMin, x); }

uint32_t fas_c_enum_sum(void) {
  return (uint32_t)fas_enum_values[0] + (uint32_t)fas_enum_values[1]
       + (uint32_t)fas_enum_values[2] + (uint32_t)fas_enum_values[3];
}

size_t fas_c_packed_size(void) { return sizeof(struct FasPackedStorage); }
size_t fas_c_packed_align(void) { return _Alignof(struct FasPackedStorage); }
size_t fas_c_packed_word_offset(void) {
  return offsetof(struct FasPackedStorage, word);
}

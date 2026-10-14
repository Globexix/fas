#include <immintrin.h>
#include <stdint.h>
#include "intrinsics.h"
uint64_t fas_opaque(uint64_t value) { __asm__("" : "+r"(value)); return value; }
uint64_t fas_bzhi(uint64_t value, uint32_t index) { return _bzhi_u64(value, index); }

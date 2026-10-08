#include <stdint.h>

uint32_t oracle_clz(void) { return (uint32_t)(__builtin_clz(1u) - 24); }

uint32_t oracle_rotr(void) { return 128u; }

uint32_t oracle_sat(void) { return 2u; }

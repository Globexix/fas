#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

void machine_integers_unsigned(uint64_t a, uint64_t b, uint64_t c, uint64_t d,
                               uint64_t e, uint64_t f, uint64_t g, uint64_t h) {
  printf("%" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64
         " %" PRIu64 " %" PRIu64 "\n",
         a, b, c, d, e, f, g, h);
}

void machine_integers_signed(int64_t a, int64_t b, int64_t c, int64_t d) {
  printf("%" PRId64 " %" PRId64 " %" PRId64 " %" PRId64 "\n", a, b, c, d);
}

void machine_integers_bits(uint32_t bits) {
  printf("%" PRIu32 "\n", bits);
}

#ifdef MACHINE_INTEGERS_ORACLE
int main(void) {
  uint64_t top = UINT64_MAX;
  uint64_t half = UINT64_C(1) << 63;
  machine_integers_unsigned(top, (uint64_t)(top + 3), (uint64_t)(top + 2),
                            (uint64_t)(half + half), top, 1, 2, top);
  machine_integers_signed(INT64_MAX, INT64_MIN + 1, -1, 3);
  machine_integers_bits(1 + 4 + 8 + 16);
  return 0;
}
#endif

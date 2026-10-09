#include <math.h>
#include <stdint.h>
#include <string.h>

static uint64_t bits(double value) {
  uint64_t result;
  memcpy(&result, &value, sizeof(result));
  return result;
}

uint64_t fas_oracle_sqrt(void) { return bits(sqrt(4.0)); }
uint64_t fas_oracle_fma(void) { return bits(fma(1.125, 2.25, -0.5)); }
uint64_t fas_oracle_floor(void) { return bits(floor(-2.75)); }
uint64_t fas_oracle_ceil(void) { return bits(ceil(-2.75)); }
uint64_t fas_oracle_round(void) { return bits(rint(2.5)); }
uint64_t fas_oracle_abs(void) { return bits(fabs(-2.5)); }

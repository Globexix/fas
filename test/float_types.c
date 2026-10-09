#include <stdint.h>
#include <string.h>

struct Pair {
  float first;
  uint32_t tag;
  double second;
  float samples[3];
};

static uint32_t bits32(float value) {
  uint32_t bits;
  memcpy(&bits, &value, sizeof(bits));
  return bits;
}

static uint64_t bits64(double value) {
  uint64_t bits;
  memcpy(&bits, &value, sizeof(bits));
  return bits;
}

int32_t float_type_check(void *pair_pointer, void *global_pointer, void *samples_pointer) {
  const struct Pair *pair = pair_pointer;
  const double *global = global_pointer;
  const float *samples = samples_pointer;
  float expected_first = 0.1f;
  double expected_second = 6.5;
  if (bits32(pair->first) != bits32(expected_first)) return 1;
  if (pair->tag != 7u) return 2;
  if (bits64(pair->second) != bits64(expected_second)) return 3;
  if (bits32(pair->samples[0]) != bits32(3.25f)) return 4;
  if (bits32(pair->samples[1]) != bits32(5.25f)) return 5;
  if (bits32(pair->samples[2]) != bits32(6.25f)) return 6;
  if (bits64(*global) != bits64(expected_second)) return 7;
  if (bits32(samples[0]) != bits32(3.25f)) return 8;
  if (bits32(samples[1]) != bits32(4.5f)) return 9;
  if (bits32(samples[2]) != bits32(5.75f)) return 10;
  return 0;
}

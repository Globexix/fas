#include <stdint.h>
#include <stdio.h>
#include <string.h>

enum { value_count = 20, slot_count = 15, mixed_slot_count = 15 };

static const uint32_t values32[value_count] = {
    0x00000000, 0x80000000, 0x00000001, 0x80000001, 0x007fffff, 0x807fffff,
    0x00800000, 0x80800000, 0x7f7fffff, 0xff7fffff, 0x7f800000, 0xff800000,
    0x7fc12345, 0x3f800000, 0x3dcccccd, 0x33800000, 0x33000000, 0x3f000000,
    0x3f800001, 0x3f7ffffe};

static const uint64_t values64[value_count] = {
    UINT64_C(0x0000000000000000), UINT64_C(0x8000000000000000),
    UINT64_C(0x0000000000000001), UINT64_C(0x8000000000000001),
    UINT64_C(0x000fffffffffffff), UINT64_C(0x800fffffffffffff),
    UINT64_C(0x0010000000000000), UINT64_C(0x8010000000000000),
    UINT64_C(0x7fefffffffffffff), UINT64_C(0xffefffffffffffff),
    UINT64_C(0x7ff0000000000000), UINT64_C(0xfff0000000000000),
    UINT64_C(0x7ff8123456789abc), UINT64_C(0x3ff0000000000000),
    UINT64_C(0x3fb999999999999a), UINT64_C(0x3ca0000000000000),
    UINT64_C(0x3c90000000000000), UINT64_C(0x3fe0000000000000),
    UINT64_C(0x3ff0000000000001), UINT64_C(0x3feffffffffffffe)};

void float_fill32(uint32_t *values) { memcpy(values, values32, sizeof values32); }
void float_fill64(uint64_t *values) { memcpy(values, values64, sizeof values64); }

static int quiet_nan32(uint32_t bits) {
  return (bits & UINT32_C(0x7f800000)) == UINT32_C(0x7f800000) &&
         (bits & UINT32_C(0x007fffff)) != 0 &&
         (bits & UINT32_C(0x00400000)) != 0;
}

static int quiet_nan64(uint64_t bits) {
  return (bits & UINT64_C(0x7ff0000000000000)) ==
             UINT64_C(0x7ff0000000000000) &&
         (bits & UINT64_C(0x000fffffffffffff)) != 0 &&
         (bits & UINT64_C(0x0008000000000000)) != 0;
}

static uint32_t bits32(float value) {
  uint32_t bits;
  memcpy(&bits, &value, sizeof bits);
  return bits;
}

static uint64_t bits64(double value) {
  uint64_t bits;
  memcpy(&bits, &value, sizeof bits);
  return bits;
}

static int compare32(size_t index, size_t slot, uint32_t actual, float expected,
                     const char *operation) {
  uint32_t expected_bits = bits32(expected);
  if (actual == expected_bits ||
      (quiet_nan32(expected_bits) && quiet_nan32(actual)))
    return 0;
  fprintf(stderr,
          "f32 %s at pair %zu slot %zu: got 0x%08x, expected 0x%08x\n",
          operation, index, slot, actual, expected_bits);
  return 1;
}

static int compare64(size_t index, size_t slot, uint64_t actual, double expected,
                     const char *operation) {
  uint64_t expected_bits = bits64(expected);
  if (actual == expected_bits ||
      (quiet_nan64(expected_bits) && quiet_nan64(actual)))
    return 0;
  fprintf(stderr,
          "f64 %s at pair %zu slot %zu: got 0x%016llx, expected 0x%016llx\n",
          operation, index, slot, (unsigned long long)actual,
          (unsigned long long)expected_bits);
  return 1;
}

int float_check32(const uint32_t *input, const uint32_t *output) {
  int failures = 0;
  for (size_t i = 0; i < value_count; ++i) {
    float a;
    memcpy(&a, &input[i], sizeof a);
    for (size_t j = 0; j < value_count; ++j) {
      float b;
      memcpy(&b, &input[j], sizeof b);
      size_t pair = i * value_count + j;
      size_t base = pair * slot_count;
      volatile float added = a + b;
      volatile float subtracted = a - b;
      volatile float multiplied = a * b;
      volatile float divided = a / b;
      volatile float compound = a;
      compound += b;
      failures += compare32(pair, 0, output[base], added, "add");
      failures += compare32(pair, 1, output[base + 1], subtracted, "sub");
      failures += compare32(pair, 2, output[base + 2], multiplied, "mul");
      failures += compare32(pair, 3, output[base + 3], divided, "div");
      failures += compare32(pair, 4, output[base + 4], -a, "neg");
      failures += compare32(pair, 5, output[base + 5], a == b ? 1.0f : 0.0f, "eq");
      failures += compare32(pair, 6, output[base + 6], a != b ? 1.0f : 0.0f, "ne");
      failures += compare32(pair, 7, output[base + 7], a < b ? 1.0f : 0.0f, "lt");
      failures += compare32(pair, 8, output[base + 8], a <= b ? 1.0f : 0.0f, "le");
      failures += compare32(pair, 9, output[base + 9], a > b ? 1.0f : 0.0f, "gt");
      failures += compare32(pair, 10, output[base + 10], a >= b ? 1.0f : 0.0f, "ge");
      failures += compare32(pair, 11, output[base + 11], compound, "add-assign");
      compound = a;
      compound -= b;
      failures += compare32(pair, 12, output[base + 12], compound, "sub-assign");
      compound = a;
      compound *= b;
      failures += compare32(pair, 13, output[base + 13], compound, "mul-assign");
      compound = a;
      compound /= b;
      failures += compare32(pair, 14, output[base + 14], compound, "div-assign");
    }
  }
  size_t separate_index = value_count * value_count * slot_count;
  float a;
  float b;
  memcpy(&a, &input[18], sizeof a);
  memcpy(&b, &input[19], sizeof b);
  volatile float product = a * b;
  volatile float separate = product - 1.0f;
  failures += compare32(value_count * value_count, 0, output[separate_index],
                        separate, "mul-add-separate");
  return failures == 0 ? 0 : 1;
}

int float_check64(const uint64_t *input, const uint64_t *output) {
  int failures = 0;
  for (size_t i = 0; i < value_count; ++i) {
    double a;
    memcpy(&a, &input[i], sizeof a);
    for (size_t j = 0; j < value_count; ++j) {
      double b;
      memcpy(&b, &input[j], sizeof b);
      size_t pair = i * value_count + j;
      size_t base = pair * slot_count;
      volatile double added = a + b;
      volatile double subtracted = a - b;
      volatile double multiplied = a * b;
      volatile double divided = a / b;
      volatile double compound = a;
      compound += b;
      failures += compare64(pair, 0, output[base], added, "add");
      failures += compare64(pair, 1, output[base + 1], subtracted, "sub");
      failures += compare64(pair, 2, output[base + 2], multiplied, "mul");
      failures += compare64(pair, 3, output[base + 3], divided, "div");
      failures += compare64(pair, 4, output[base + 4], -a, "neg");
      failures += compare64(pair, 5, output[base + 5], a == b ? 1.0 : 0.0, "eq");
      failures += compare64(pair, 6, output[base + 6], a != b ? 1.0 : 0.0, "ne");
      failures += compare64(pair, 7, output[base + 7], a < b ? 1.0 : 0.0, "lt");
      failures += compare64(pair, 8, output[base + 8], a <= b ? 1.0 : 0.0, "le");
      failures += compare64(pair, 9, output[base + 9], a > b ? 1.0 : 0.0, "gt");
      failures += compare64(pair, 10, output[base + 10], a >= b ? 1.0 : 0.0, "ge");
      failures += compare64(pair, 11, output[base + 11], compound, "add-assign");
      compound = a;
      compound -= b;
      failures += compare64(pair, 12, output[base + 12], compound, "sub-assign");
      compound = a;
      compound *= b;
      failures += compare64(pair, 13, output[base + 13], compound, "mul-assign");
      compound = a;
      compound /= b;
      failures += compare64(pair, 14, output[base + 14], compound, "div-assign");
    }
  }
  size_t separate_index = value_count * value_count * slot_count;
  double a;
  double b;
  memcpy(&a, &input[18], sizeof a);
  memcpy(&b, &input[19], sizeof b);
  volatile double product = a * b;
  volatile double separate = product - 1.0;
  failures += compare64(value_count * value_count, 0, output[separate_index],
                        separate, "mul-add-separate");
  return failures == 0 ? 0 : 1;
}

int float_check_mixed(const uint32_t *input32, const uint64_t *input64,
                      const uint64_t *output) {
  int failures = 0;
  for (size_t i = 0; i < value_count; ++i) {
    float source_a;
    memcpy(&source_a, &input32[i], sizeof source_a);
    double a = source_a;
    for (size_t j = 0; j < value_count; ++j) {
      double b;
      memcpy(&b, &input64[j], sizeof b);
      size_t pair = i * value_count + j;
      size_t base = pair * mixed_slot_count;
      volatile double added = a + b;
      volatile double subtracted = a - b;
      volatile double multiplied = a * b;
      volatile double divided = a / b;
      volatile double compound = b;
      compound += a;
      failures += compare64(pair, 0, output[base], added, "mixed-add");
      failures += compare64(pair, 1, output[base + 1], subtracted, "mixed-sub");
      failures += compare64(pair, 2, output[base + 2], multiplied, "mixed-mul");
      failures += compare64(pair, 3, output[base + 3], divided, "mixed-div");
      failures += compare64(pair, 4, output[base + 4], a == b ? 1.0 : 0.0, "mixed-eq");
      failures += compare64(pair, 5, output[base + 5], a != b ? 1.0 : 0.0, "mixed-ne");
      failures += compare64(pair, 6, output[base + 6], a < b ? 1.0 : 0.0, "mixed-lt");
      failures += compare64(pair, 7, output[base + 7], a <= b ? 1.0 : 0.0, "mixed-le");
      failures += compare64(pair, 8, output[base + 8], a > b ? 1.0 : 0.0, "mixed-gt");
      failures += compare64(pair, 9, output[base + 9], a >= b ? 1.0 : 0.0, "mixed-ge");
      failures += compare64(pair, 10, output[base + 10], source_a != 0.0f ? a : b,
                            "mixed-if");
      failures += compare64(pair, 11, output[base + 11], compound, "mixed-add-assign");
      compound = b;
      compound -= a;
      failures += compare64(pair, 12, output[base + 12], compound, "mixed-sub-assign");
      compound = b;
      compound *= a;
      failures += compare64(pair, 13, output[base + 13], compound, "mixed-mul-assign");
      compound = b;
      compound /= a;
      failures += compare64(pair, 14, output[base + 14], compound, "mixed-div-assign");
    }
  }
  return failures == 0 ? 0 : 1;
}

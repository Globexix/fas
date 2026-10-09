#define _GNU_SOURCE
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static const uint32_t edge32[] = {
    0x00000000, 0x80000000, 0x00000001, 0x80000001, 0x007fffff, 0x807fffff,
    0x00800000, 0x80800000, 0x7f7fffff, 0xff7fffff, 0x7f800000, 0xff800000,
    0x7fc00001, 0xffc12345, 0x3f000000, 0xbf000000, 0x3fc00000, 0xbfc00000,
    0x40200000, 0xc0200000, 0x4b000000, 0x4b000001, 0x4b800000, 0x4b800001};
static const uint64_t edge64[] = {
    0x0000000000000000ULL, 0x8000000000000000ULL, 0x0000000000000001ULL,
    0x8000000000000001ULL, 0x000fffffffffffffULL, 0x800fffffffffffffULL,
    0x0010000000000000ULL, 0x8010000000000000ULL, 0x7fefffffffffffffULL,
    0xffefffffffffffffULL, 0x7ff0000000000000ULL, 0xfff0000000000000ULL,
    0x7ff8000000000001ULL, 0xfff8123456789abcULL, 0x3fe0000000000000ULL,
    0xbfe0000000000000ULL, 0x3ff8000000000000ULL, 0xbff8000000000000ULL,
    0x4004000000000000ULL, 0xc004000000000000ULL, 0x4330000000000000ULL,
    0x4330000000000001ULL, 0x4340000000000000ULL, 0x4340000000000001ULL};
static const uint32_t fma32_a[] = {
    0xd0300000, 0xe36967c8, 0x3a6549a8, 0x53008000,
    0xebe963c8, 0x9c61d300, 0x48500000, 0xf3bd2b68};
static const uint32_t fma32_b[] = {
    0xd63d6b18, 0x3d900000, 0xf9500000, 0x23149180,
    0x35900000, 0x66318000, 0x4254a128, 0x8cd00000};
static const uint32_t fma32_c[] = {
    0x10948057, 0x8f93c7c8, 0xd35fe9a8, 0x95bdeb52,
    0x879343c8, 0x9004c818, 0x18488810, 0x16d88b68};
static const uint64_t fma64_a[] = {0x61c37a4b613268b8ULL};
static const uint64_t fma64_b[] = {0x0a14eebbb2af2269ULL};
static const uint64_t fma64_c[] = {0xa9530b400695c66dULL};

static float f32(uint32_t bits) {
  float value;
  memcpy(&value, &bits, sizeof(value));
  return value;
}
static double f64(uint64_t bits) {
  double value;
  memcpy(&value, &bits, sizeof(value));
  return value;
}
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

uint32_t float_vectors_expect32(uint32_t op, uint32_t a_bits, uint32_t b_bits,
                                uint32_t c_bits) {
  float a = f32(a_bits), b = f32(b_bits), c = f32(c_bits);
  switch (op) {
  case 0: return bits32(a + b);
  case 1: return bits32(a - b);
  case 2: return bits32(a * b);
  case 3: return bits32(a / b);
  case 4: return bits32(-a);
  case 5: return bits32(sqrtf(a));
  case 6: return bits32(floorf(a));
  case 7: return bits32(ceilf(a));
  case 8: return bits32(roundevenf(a));
  case 9: return bits32(fabsf(a));
  case 10: return bits32(fmaf(a, b, c));
  case 11: return a == b;
  case 12: return a != b;
  case 13: return a < b;
  case 14: return a <= b;
  case 15: return a > b;
  case 16: return a >= b;
  case 17: return a < b ? b_bits : c_bits;
  case 18: return bits32((float)(int32_t)a_bits);
  case 19:
    if (isnan(a)) return 0;
    if (a >= 2147483648.0f) return UINT32_C(0x7fffffff);
    if (a <= -2147483648.0f) return UINT32_C(0x80000000);
    return (uint32_t)(int32_t)truncf(a);
  default: return UINT32_MAX;
  }
}

uint64_t float_vectors_expect64(uint32_t op, uint64_t a_bits, uint64_t b_bits,
                                uint64_t c_bits) {
  double a = f64(a_bits), b = f64(b_bits), c = f64(c_bits);
  switch (op) {
  case 0: return bits64(a + b);
  case 1: return bits64(a - b);
  case 2: return bits64(a * b);
  case 3: return bits64(a / b);
  case 4: return bits64(-a);
  case 5: return bits64(sqrt(a));
  case 6: return bits64(floor(a));
  case 7: return bits64(ceil(a));
  case 8: return bits64(roundeven(a));
  case 9: return bits64(fabs(a));
  case 10: return bits64(fma(a, b, c));
  case 11: return a == b;
  case 12: return a != b;
  case 13: return a < b;
  case 14: return a <= b;
  case 15: return a > b;
  case 16: return a >= b;
  case 17: return a < b ? b_bits : c_bits;
  case 18: return bits64((double)a_bits);
  case 19:
    if (isnan(a) || a <= 0.0) return 0;
    if (a >= 18446744073709551616.0) return UINT64_MAX;
    return (uint64_t)trunc(a);
  case 20: return bits64((double)f32((uint32_t)a_bits));
  default: return UINT64_MAX;
  }
}

uint32_t float_vectors_narrow64(uint64_t a_bits) {
  return bits32((float)f64(a_bits));
}

typedef int (*check32_fn)(const void *, const void *, const void *, const void *, const void *);
typedef int (*check64_fn)(const void *, const void *, const void *, const void *, const void *);
int float_vectors_check32_2(const void *, const void *, const void *, const void *, const void *);
int float_vectors_check32_4(const void *, const void *, const void *, const void *, const void *);
int float_vectors_check32_8(const void *, const void *, const void *, const void *, const void *);
int float_vectors_check32_16(const void *, const void *, const void *, const void *, const void *);
int float_vectors_check64_2(const void *, const void *, const void *, const void *, const void *);
int float_vectors_check64_4(const void *, const void *, const void *, const void *, const void *);
int float_vectors_check64_8(const void *, const void *, const void *, const void *, const void *);
typedef int (*memory_fn)(const void *, void *, void *, void *, const void *);
int float_vectors_memory32_2(const void *, void *, void *, void *, const void *);
int float_vectors_memory32_4(const void *, void *, void *, void *, const void *);
int float_vectors_memory32_8(const void *, void *, void *, void *, const void *);
int float_vectors_memory32_16(const void *, void *, void *, void *, const void *);
int float_vectors_memory64_2(const void *, void *, void *, void *, const void *);
int float_vectors_memory64_4(const void *, void *, void *, void *, const void *);
int float_vectors_memory64_8(const void *, void *, void *, void *, const void *);
int float_vectors_shuffle(const void *, const void *);
int float_vectors_construction(void);

static int run32_width(check32_fn check, size_t lanes) {
  uint32_t a[16], b[16], c[16], permutation[16];
  int32_t integers[16];
  for (size_t base = 0; base < sizeof(edge32) / sizeof(edge32[0]); ++base) {
    for (size_t lane = 0; lane < lanes; ++lane) {
      size_t i = (base + lane) % (sizeof(edge32) / sizeof(edge32[0]));
      a[lane] = edge32[i];
      b[lane] = edge32[(i + 7) % (sizeof(edge32) / sizeof(edge32[0]))];
      c[lane] = edge32[(i + 13) % (sizeof(edge32) / sizeof(edge32[0]))];
      integers[lane] = (int32_t)edge32[(i + 5) % (sizeof(edge32) / sizeof(edge32[0]))];
      permutation[lane] = (uint32_t)((lane * 3 + 1) % lanes);
    }
    int result = check(a, b, c, integers, permutation);
    if (result) {
      fprintf(stderr, "float vectors f32x%zu edge %zu operation %d\n", lanes, base, result);
      return 1;
    }
  }
  for (size_t base = 0; base < sizeof(fma32_a) / sizeof(fma32_a[0]); ++base) {
    for (size_t lane = 0; lane < lanes; ++lane) {
      size_t i = (base + lane) % (sizeof(fma32_a) / sizeof(fma32_a[0]));
      a[lane] = fma32_a[i]; b[lane] = fma32_b[i]; c[lane] = fma32_c[i];
      integers[lane] = 0; permutation[lane] = (uint32_t)lane;
    }
    if (check(a, b, c, integers, permutation)) {
      fprintf(stderr, "float vectors f32x%zu single-rounding case %zu failed\n", lanes, base);
      return 1;
    }
  }
  return 0;
}

static int run64_width(check64_fn check, size_t lanes) {
  uint64_t a[8], b[8], c[8], integers[8];
  uint32_t permutation[8];
  for (size_t base = 0; base < sizeof(edge64) / sizeof(edge64[0]); ++base) {
    for (size_t lane = 0; lane < lanes; ++lane) {
      size_t i = (base + lane) % (sizeof(edge64) / sizeof(edge64[0]));
      a[lane] = edge64[i];
      b[lane] = edge64[(i + 7) % (sizeof(edge64) / sizeof(edge64[0]))];
      c[lane] = edge64[(i + 13) % (sizeof(edge64) / sizeof(edge64[0]))];
      integers[lane] = edge64[(i + 5) % (sizeof(edge64) / sizeof(edge64[0]))];
      permutation[lane] = (uint32_t)((lane * 3 + 1) % lanes);
    }
    int result = check(a, b, c, integers, permutation);
    if (result) {
      fprintf(stderr, "float vectors f64x%zu edge %zu operation %d\n", lanes, base, result);
      return 1;
    }
  }
  for (size_t lane = 0; lane < lanes; ++lane) {
    a[lane] = fma64_a[0]; b[lane] = fma64_b[0]; c[lane] = fma64_c[0];
    integers[lane] = 0; permutation[lane] = (uint32_t)lane;
  }
  if (check(a, b, c, integers, permutation)) {
    fprintf(stderr, "float vectors f64x%zu single-rounding case failed\n", lanes);
    return 1;
  }
  return 0;
}


static int run_memory32(memory_fn check, size_t lanes) {
  float input[16], masked[16] = {0}, scattered[16] = {0}, bytes[16] = {0};
  size_t indices[16];
  for (size_t i = 0; i < lanes; ++i) {
    input[i] = f32(edge32[(i * 3 + 8) % (sizeof(edge32) / sizeof(edge32[0]))]);
    indices[i] = lanes - 1 - i;
  }
  int result = check(input, masked, scattered, bytes, indices);
  if (result) {
    fprintf(stderr, "float vectors f32 memory x%zu operation %d\n", lanes, result);
    return 1;
  }
  for (size_t i = 0; i < lanes; ++i) {
    if (bits32(masked[i]) != bits32(input[i])) return 2;
    if (bits32(scattered[indices[i]]) != bits32(input[i])) return 3;
    if (bits32(bytes[i]) != bits32(input[i])) return 4;
  }
  return 0;
}
static int run_memory64(memory_fn check, size_t lanes) {
  double input[8], masked[8] = {0}, scattered[8] = {0}, bytes[8] = {0};
  size_t indices[8];
  for (size_t i = 0; i < lanes; ++i) {
    input[i] = f64(edge64[(i * 3 + 8) % (sizeof(edge64) / sizeof(edge64[0]))]);
    indices[i] = lanes - 1 - i;
  }
  int result = check(input, masked, scattered, bytes, indices);
  if (result) {
    fprintf(stderr, "float vectors f64 memory x%zu operation %d\n", lanes, result);
    return 1;
  }
  for (size_t i = 0; i < lanes; ++i) {
    if (bits64(masked[i]) != bits64(input[i])) return 2;
    if (bits64(scattered[indices[i]]) != bits64(input[i])) return 3;
    if (bits64(bytes[i]) != bits64(input[i])) return 4;
  }
  return 0;
}

static int fma_cases_are_single_rounding(void) {
  for (size_t i = 0; i < sizeof(fma32_a) / sizeof(fma32_a[0]); ++i) {
    float a = f32(fma32_a[i]), b = f32(fma32_b[i]), c = f32(fma32_c[i]);
    volatile double product = (double)a * (double)b;
    volatile double sum = product + (double)c;
    if (bits32(fmaf(a, b, c)) == bits32((float)sum)) return 0;
  }
  double a = f64(fma64_a[0]), b = f64(fma64_b[0]), c = f64(fma64_c[0]);
  volatile double product = a * b;
  volatile double sum = product + c;
  return bits64(fma(a, b, c)) != bits64(sum);
}

int main(void) {
  if (!fma_cases_are_single_rounding()) { fprintf(stderr, "fma cases check failed\n"); return 1; }
  if (run32_width(float_vectors_check32_2, 2) || run32_width(float_vectors_check32_4, 4) ||
      run32_width(float_vectors_check32_8, 8) || run32_width(float_vectors_check32_16, 16)) return 2;
  if (run64_width(float_vectors_check64_2, 2) || run64_width(float_vectors_check64_4, 4) ||
      run64_width(float_vectors_check64_8, 8)) return 3;
  if (run_memory32(float_vectors_memory32_2, 2) || run_memory32(float_vectors_memory32_4, 4) ||
      run_memory32(float_vectors_memory32_8, 8) || run_memory32(float_vectors_memory32_16, 16)) return 4;
  if (run_memory64(float_vectors_memory64_2, 2) || run_memory64(float_vectors_memory64_4, 4) ||
      run_memory64(float_vectors_memory64_8, 8)) return 5;
  float shuffle_a[4] = {1, 2, 3, 4}, shuffle_b[4] = {5, 6, 7, 8};
  if (float_vectors_shuffle(shuffle_a, shuffle_b)) return 6;
  int construction_result = float_vectors_construction();
  if (construction_result) { fprintf(stderr, "construction helper %d\n", construction_result); return 7; }
  puts("float vectors: C scalar oracle, edge sets, conversions, memory and single-rounding fma: ok");
  return 0;
}

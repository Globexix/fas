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

static float float32(uint32_t bits) {
  float value;
  memcpy(&value, &bits, sizeof(value));
  return value;
}

static double float64(uint64_t bits) {
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

static uint32_t next32(uint32_t *state) {
  uint32_t value = *state;
  value ^= value << 13;
  value ^= value >> 17;
  value ^= value << 5;
  return *state = value;
}

static uint64_t next64(uint64_t *state) {
  uint64_t value = *state;
  value ^= value << 13;
  value ^= value >> 7;
  value ^= value << 17;
  return *state = value;
}

static void show32(uint32_t bits) {
  float value = float32(bits);
  printf("32 %08x %08x %08x %08x %08x %08x\n", bits,
         bits32(sqrtf(value)), bits32(floorf(value)), bits32(ceilf(value)),
         bits32(roundevenf(value)), bits32(fabsf(value)));
}

static void show64(uint64_t bits) {
  double value = float64(bits);
  printf("64 %016lx %016lx %016lx %016lx %016lx %016lx\n",
         (unsigned long)bits, (unsigned long)bits64(sqrt(value)),
         (unsigned long)bits64(floor(value)), (unsigned long)bits64(ceil(value)),
         (unsigned long)bits64(roundeven(value)), (unsigned long)bits64(fabs(value)));
}

static void show_fma32(uint32_t a_bits, uint32_t b_bits, uint32_t c_bits) {
  printf("fma32 %08x %08x %08x %08x\n", a_bits, b_bits, c_bits,
         bits32(fmaf(float32(a_bits), float32(b_bits), float32(c_bits))));
}

static void show_fma64(uint64_t a_bits, uint64_t b_bits, uint64_t c_bits) {
  printf("fma64 %016lx %016lx %016lx %016lx\n", (unsigned long)a_bits,
         (unsigned long)b_bits, (unsigned long)c_bits,
         (unsigned long)bits64(fma(float64(a_bits), float64(b_bits), float64(c_bits))));
}

static int fma32_cases_are_single_rounding_cases(void) {
  for (size_t i = 0; i < sizeof(fma32_a) / sizeof(fma32_a[0]); ++i) {
    float a = float32(fma32_a[i]);
    float b = float32(fma32_b[i]);
    float c = float32(fma32_c[i]);
    volatile double product = (double)a * (double)b;
    volatile double sum = product + (double)c;
    if (bits32(fmaf(a, b, c)) == bits32((float)sum)) return 0;
  }
  return 1;
}

int main(void) {
  if (!fma32_cases_are_single_rounding_cases()) return 2;
  printf("const32 %08x %08x %08x %08x %08x %08x %08x\n",
         bits32(sqrtf(2.0f)), bits32(fmaf(float32(0xd0300000), float32(0xd63d6b18),
                                              float32(0x10948057))),
         bits32(floorf(-0.25f)), bits32(ceilf(-0.25f)), bits32(roundevenf(2.5f)),
         bits32(roundevenf(-0.5f)), bits32(fabsf(float32(0xffc12345))));
  printf("const64 %016lx %016lx %016lx %016lx %016lx %016lx\n",
         (unsigned long)bits64(sqrt(2.0)),
         (unsigned long)bits64(fma(1.25, 2.5, -0.75)),
         (unsigned long)bits64(floor(-0.25)), (unsigned long)bits64(ceil(-0.25)),
         (unsigned long)bits64(roundeven(2.5)),
         (unsigned long)bits64(fabs(float64(0xfff8123456789abcULL))));
  for (size_t i = 0; i < sizeof(edge32) / sizeof(edge32[0]); ++i) show32(edge32[i]);
  uint32_t state32 = 0x9e3779b9;
  for (unsigned i = 0; i < 2048; ++i) show32(next32(&state32));
  for (size_t i = 0; i < sizeof(edge64) / sizeof(edge64[0]); ++i) show64(edge64[i]);
  uint64_t state64 = 0x9e3779b97f4a7c15ULL;
  for (unsigned i = 0; i < 2048; ++i) show64(next64(&state64));
  for (size_t i = 0; i < sizeof(fma32_a) / sizeof(fma32_a[0]); ++i)
    show_fma32(fma32_a[i], fma32_b[i], fma32_c[i]);
  for (size_t i = 0; i < sizeof(edge32) / sizeof(edge32[0]); ++i)
    show_fma32(edge32[i], 0x3f800000, 0);
  state32 = 0x6d2b79f5;
  for (unsigned i = 0; i < 2048; ++i) {
    uint32_t a = next32(&state32);
    uint32_t b = next32(&state32);
    show_fma32(a, b, next32(&state32));
  }
  for (size_t i = 0; i < sizeof(edge64) / sizeof(edge64[0]); ++i)
    show_fma64(edge64[i], 0x3ff0000000000000ULL, 0);
  state64 = 0xd1b54a32d192ed03ULL;
  for (unsigned i = 0; i < 2048; ++i) {
    uint64_t a = next64(&state64);
    uint64_t b = next64(&state64);
    show_fma64(a, b, next64(&state64));
  }
  return 0;
}

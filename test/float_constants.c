#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

extern void float_constants(uint64_t *output);
extern void float_runtime(float *input32, double *input64, uint64_t *output);

static uint64_t bits32(float value) {
  uint32_t bits;
  memcpy(&bits, &value, sizeof bits);
  return bits;
}

static uint64_t bits64(double value) {
  uint64_t bits;
  memcpy(&bits, &value, sizeof bits);
  return bits;
}

static float add32(float left, float right) {
  volatile float result = left + right;
  return result;
}

static float subtract32(float left, float right) {
  volatile float result = left - right;
  return result;
}

static float multiply32(float left, float right) {
  volatile float result = left * right;
  return result;
}

static float divide32(float left, float right) {
  volatile float result = left / right;
  return result;
}

static double add64(double left, double right) {
  volatile double result = left + right;
  return result;
}

static double subtract64(double left, double right) {
  volatile double result = left - right;
  return result;
}

static double multiply64(double left, double right) {
  volatile double result = left * right;
  return result;
}

static double divide64(double left, double right) {
  volatile double result = left / right;
  return result;
}

int main(void) {
  float input32[16] = {1.0f, 0x1p-24f, FLT_MAX, FLT_MAX, FLT_TRUE_MIN,
                       0.5f, 1.0f, 0.0f, 0.0f, 0.0f, NAN, NAN,
                       NAN, 1.0f, -0.0f, 0.0f};
  double input64[16] = {1.0, 0x1p-53, DBL_MAX, DBL_MAX, DBL_TRUE_MIN,
                        0.5, 1.0, 0.0, 0.0, 0.0, NAN, NAN,
                        NAN, 1.0, -0.0, 0.0};
  uint64_t constants[56];
  uint64_t runtime[29];
  uint64_t expected[16] = {
      bits32(add32(1.0f, 0x1p-24f)),
      bits32(add32(FLT_MAX, FLT_MAX)),
      bits32(multiply32(FLT_TRUE_MIN, 0.5f)),
      bits32(divide32(1.0f, 0.0f)),
      bits32(-0.0f),
      1,
      0,
      1,
      bits64(add64(1.0, 0x1p-53)),
      bits64(add64(DBL_MAX, DBL_MAX)),
      bits64(multiply64(DBL_TRUE_MIN, 0.5)),
      bits64(divide64(1.0, 0.0)),
      bits64(-0.0),
      1,
      0,
      1};
  float_constants(constants);
  float_runtime(input32, input64, runtime);
  for (size_t i = 0; i < 16; ++i) {
    if (constants[i] != expected[i] || runtime[i] != expected[i]) {
      fprintf(stderr,
              "float constants differ at %zu: const 0x%016llx runtime "
              "0x%016llx expected 0x%016llx\n",
              i, (unsigned long long)constants[i],
              (unsigned long long)runtime[i],
              (unsigned long long)expected[i]);
      return 1;
    }
  }
  uint64_t aggregate_expected[5] = {
      bits32(-0.0f), bits32(FLT_TRUE_MIN), bits32(1.0f),
      bits32(add32(1.0f, 0x1p-24f)), bits64(add64(1.0, 0x1p-53))};
  for (size_t i = 0; i < 5; ++i) {
    if (constants[16 + i] != aggregate_expected[i]) {
      fprintf(stderr, "float constant aggregate differs at %zu\n", i);
      return 1;
    }
  }
  uint64_t conversion_expected[10] = {
      bits32((float)INT64_C(9007199254740993)),
      bits32((float)UINT64_MAX),
      bits64((double)UINT64_MAX),
      bits64((double)FLT_TRUE_MIN),
      bits32(1.0f),
      0,
      UINT8_MAX,
      (uint64_t)(int64_t)INT8_MIN,
      INT8_MAX,
      UINT64_MAX};
  for (size_t i = 0; i < 10; ++i) {
    if (constants[21 + i] != conversion_expected[i]) {
      fprintf(stderr,
              "float constant conversion differs at %zu: got 0x%016llx "
              "expected 0x%016llx\n",
              i, (unsigned long long)constants[21 + i],
              (unsigned long long)conversion_expected[i]);
      return 1;
    }
  }
  uint64_t extra_expected[13] = {
      bits32(add32(add32(1.0f, 0x1p-24f), 0x1p-24f)),
      bits64(add64(add64(1.0, 0x1p-53), 0x1p-53)),
      bits32(subtract32(FLT_TRUE_MIN, FLT_TRUE_MIN)),
      bits64(subtract64(DBL_TRUE_MIN, DBL_TRUE_MIN)),
      1,
      1,
      1,
      1,
      1,
      1,
      bits64((double)FLT_TRUE_MIN),
      bits64(add64((double)FLT_TRUE_MIN, 1.0)),
      bits32(add32(1.0f, 0x1p-24f))};
  for (size_t i = 0; i < 13; ++i) {
    if (constants[31 + i] != extra_expected[i] || runtime[16 + i] != extra_expected[i]) {
      fprintf(stderr,
              "float constant parity differs at %zu: const 0x%016llx runtime "
              "0x%016llx expected 0x%016llx\n",
              i, (unsigned long long)constants[31 + i],
              (unsigned long long)runtime[16 + i],
              (unsigned long long)extra_expected[i]);
      return 1;
    }
  }
  const uint64_t global_expected[12] = {
      UINT32_C(0x007fffff), UINT32_C(0x00800000), UINT32_C(0x7f800000),
      UINT32_C(0x7fc00000), UINT64_C(0x8000000000000000), UINT64_C(1),
      UINT64_C(0x7ff0000000000000), UINT64_C(0x7ff8000000000000),
      UINT64_C(0x8000000000000000), UINT64_C(1),
      UINT64_C(0x7ff0000000000000), UINT64_C(0x7ff8000000000000)};
  for (size_t i = 0; i < 12; ++i) {
    if (constants[44 + i] != global_expected[i]) {
      fprintf(stderr, "float global aggregate differs at %zu\n", i);
      return 1;
    }
  }
  return 0;
}

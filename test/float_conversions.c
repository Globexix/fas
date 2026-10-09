#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

enum { count = 30, integer_kind_count = 10 };

typedef void (*convert_fn)(void *, void *);
typedef void (*float_convert_fn)(void *, void *, void *);

static const int8_t i8_values[count] = {
    INT8_MIN, -127, -64, -3, -1, 0, 1, 2, 63, 126, INT8_MAX, 42,
    -120, -100, -80, -60, -40, -20, -10, -5, 5, 10, 20, 40, 60, 80, 100,
    120, 17, -17};
static const uint8_t u8_values[count] = {
    0, 1, 2, 3, 42, 63, 127, 128, 200, 253, 254, UINT8_MAX,
    4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21};
static const int16_t i16_values[count] = {
    INT16_MIN, -32767, -12345, -3, -1, 0, 1, 2, 12345, 32766, INT16_MAX, 167,
    -30000, -20000, -10000, -5000, -1000, -500, -100, -50, 50, 100, 500, 1000,
    5000, 10000, 20000, 30000, 2345, -2345};
static const uint16_t u16_values[count] = {
    0, 1, 2, 3, 42, 32767, 32768, 50000, 65500, 65533, 65534, UINT16_MAX,
    4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21};
static const int32_t i32_values[count] = {
    INT32_MIN, INT32_MIN + 1, -16777217, -1, 0, 1, 16777215, 16777216,
    16777217, 2147483000, INT32_MAX - 1, INT32_MAX, -1000000000, -1000000,
    -10000, -100, -2, 2, 100, 10000, 1000000, 1000000000, 123456789,
    -123456789, 33554431, 33554432, 33554433, -33554431, -33554432,
    -33554433};
static const uint32_t u32_values[count] = {
    0, 1, 2, 16777215, 16777216, 16777217, 2147483647, 2147483648u,
    3000000000u, UINT32_MAX - 2, UINT32_MAX - 1, UINT32_MAX, 3, 4, 5, 6, 7,
    8, 9, 10, 100, 10000, 1000000, 1000000000, 123456789, 33554431,
    33554432, 33554433, 4000000000u, UINT32_C(4294967000)};
static const int64_t i64_values[count] = {
    INT64_MIN, INT64_MIN + 1, -INT64_C(9007199254740993),
    -INT64_C(1152921573326323713), -INT64_C(16777217), -1, 0, 1,
    INT64_C(16777217), INT64_C(9007199254740993),
    INT64_C(1152921573326323713), INT64_MAX, -INT64_C(1234567890123456789),
    -INT64_C(10000000000000001), -INT64_C(1000000000000000),
    -INT64_C(1000000000000), -INT64_C(1000000000), -INT64_C(1000000),
    -10000, -100, -2, 2, 100, 10000, INT64_C(1000000000),
    INT64_C(1000000000000), INT64_C(1000000000000000),
    INT64_C(10000000000000001), INT64_C(1234567890123456789),
    INT64_C(2000000000000000000)};
static const uint64_t u64_values[count] = {
    0, 1, 2, UINT64_C(16777217), UINT64_C(9007199254740993),
    UINT64_C(1152921573326323713), UINT64_C(9223372036854775808),
    UINT64_C(12345678901234567890), UINT64_MAX - 3, UINT64_MAX - 2,
    UINT64_MAX - 1, UINT64_MAX, 3, 4, 5, 6, 7, 8, 9, 10, 100,
    UINT64_C(1000000000000), UINT64_C(1000000000000000),
    UINT64_C(10000000000000001), UINT64_C(1234567890123456789),
    UINT64_C(1844674407370955161), UINT64_C(9223372036854775807),
    UINT64_C(9007199254740991), UINT64_C(9007199254740992),
    UINT64_C(9007199254740993)};
static const int64_t isize_values[count] = {
    INT64_MIN, INT64_MIN + 1, -INT64_C(9007199254740993),
    -INT64_C(1152921573326323713), -INT64_C(16777217), -1, 0, 1,
    INT64_C(16777217), INT64_C(9007199254740993),
    INT64_C(1152921573326323713), INT64_MAX, -INT64_C(1234567890123456789),
    -INT64_C(10000000000000001), -INT64_C(1000000000000000),
    -INT64_C(1000000000000), -INT64_C(1000000000), -INT64_C(1000000),
    -10000, -100, -2, 2, 100, 10000, INT64_C(1000000000),
    INT64_C(1000000000000), INT64_C(1000000000000000),
    INT64_C(10000000000000001), INT64_C(1234567890123456789),
    INT64_C(2000000000000000000)};
static const uint64_t usize_values[count] = {
    0, 1, 2, UINT64_C(16777217), UINT64_C(9007199254740993),
    UINT64_C(1152921573326323713), UINT64_C(9223372036854775808),
    UINT64_C(12345678901234567890), UINT64_MAX - 3, UINT64_MAX - 2,
    UINT64_MAX - 1, UINT64_MAX, 3, 4, 5, 6, 7, 8, 9, 10, 100,
    UINT64_C(1000000000000), UINT64_C(1000000000000000),
    UINT64_C(10000000000000001), UINT64_C(1234567890123456789),
    UINT64_C(1844674407370955161), UINT64_C(9223372036854775807),
    UINT64_C(9007199254740991), UINT64_C(9007199254740992),
    UINT64_C(9007199254740993)};

static const float f32_values[count] = {
    -INFINITY, -1.0e30f, -4294967296.0f, -2147483648.0f, -65536.0f,
    -32768.0f, -256.0f, -128.0f, -127.9f, -1.9f, -1.0f, -0.0f,
    0.0f, 0.9f, 1.9f, 126.9f, 127.0f, 127.9f, 128.0f, 254.9f,
    255.0f, 255.9f, 256.0f, 32767.0f, 65535.0f, 2147483520.0f,
    4294967296.0f, INFINITY, NAN, FLT_TRUE_MIN};
static const double f64_values[count] = {
    -INFINITY, -1.0e300, -18446744073709551616.0, -9223372036854775808.0,
    -2147483649.0, -65536.0, -32768.0, -256.0, -128.0, -127.9, -1.9,
    -1.0, -0.0, 0.0, 0.9, 1.9, 126.9, 127.0, 127.9, 128.0, 254.9,
    255.0, 255.9, 256.0, 65535.9, 4294967295.0, 9223372036854774784.0,
    9223372036854775808.0, 18446744073709549568.0, NAN};

extern void fas_i8_float(void *, void *);
extern void fas_u8_float(void *, void *);
extern void fas_i16_float(void *, void *);
extern void fas_u16_float(void *, void *);
extern void fas_i32_float(void *, void *);
extern void fas_u32_float(void *, void *);
extern void fas_i64_float(void *, void *);
extern void fas_u64_float(void *, void *);
extern void fas_isize_float(void *, void *);
extern void fas_usize_float(void *, void *);
extern void fas_f32_int(void *, void *);
extern void fas_f64_int(void *, void *);
extern void fas_float_float(void *, void *, void *);
extern void fas_float_bits(void *, void *, void *);

static uint32_t bits32(float value) {
  uint32_t result;
  memcpy(&result, &value, sizeof result);
  return result;
}

static uint64_t bits64(double value) {
  uint64_t result;
  memcpy(&result, &value, sizeof result);
  return result;
}

static int quiet_nan32(uint32_t value) {
  return (value & UINT32_C(0x7f800000)) == UINT32_C(0x7f800000) &&
         (value & UINT32_C(0x007fffff)) != 0 &&
         (value & UINT32_C(0x00400000)) != 0;
}

static int quiet_nan64(uint64_t value) {
  return (value & UINT64_C(0x7ff0000000000000)) ==
             UINT64_C(0x7ff0000000000000) &&
         (value & UINT64_C(0x000fffffffffffff)) != 0 &&
         (value & UINT64_C(0x0008000000000000)) != 0;
}

static uint64_t mask_width(unsigned width) {
  return width == 64 ? UINT64_MAX : (UINT64_C(1) << width) - 1;
}

static uint64_t saturating_unsigned(double input, unsigned width) {
  long double value = input;
  uint64_t maximum = mask_width(width);
  if (isnan(input) || value <= 0.0L) return 0;
  if (value >= (long double)maximum) return maximum;
  return (uint64_t)value & maximum;
}

static uint64_t saturating_signed(double input, unsigned width) {
  long double value = input;
  int64_t minimum = width == 64 ? INT64_MIN : -(INT64_C(1) << (width - 1));
  int64_t maximum = width == 64 ? INT64_MAX : (INT64_C(1) << (width - 1)) - 1;
  if (isnan(input)) return 0;
  if (value <= (long double)minimum) return (uint64_t)minimum & mask_width(width);
  if (value >= (long double)maximum) return (uint64_t)maximum & mask_width(width);
  return (uint64_t)(int64_t)value & mask_width(width);
}

static const unsigned widths[integer_kind_count] = {8, 16, 32, 64, 64,
                                                     8, 16, 32, 64, 64};
static const int signed_kinds[integer_kind_count] = {0, 0, 0, 0, 0,
                                                     1, 1, 1, 1, 1};

static uint64_t expected_integer(double value, unsigned kind) {
  return signed_kinds[kind] ? saturating_signed(value, widths[kind])
                            : saturating_unsigned(value, widths[kind]);
}

static int check_integer_float(const char *name, const uint64_t *actual,
                               const long double *values, int is_signed) {
  for (size_t i = 0; i < count; ++i) {
    long double value = values[i];
    float expected32 = (float)value;
    double expected64 = (double)value;
    if ((uint32_t)actual[i * 2] != bits32(expected32) ||
        actual[i * 2 + 1] != bits64(expected64)) {
      fprintf(stderr,
              "%s to float differs at %zu: got 0x%08x/0x%016llx, expected "
              "0x%08x/0x%016llx (%s)\n",
              name, i, (uint32_t)actual[i * 2],
              (unsigned long long)actual[i * 2 + 1], bits32(expected32),
              (unsigned long long)bits64(expected64),
              is_signed ? "signed" : "unsigned");
      return 1;
    }
  }
  return 0;
}

static int check_float_int(const char *name, const double *values,
                           const uint64_t *actual) {
  for (size_t kind = 0; kind < integer_kind_count; ++kind) {
    for (size_t i = 0; i < count; ++i) {
      uint64_t expected = expected_integer(values[i], (unsigned)kind);
      uint64_t got = actual[kind * count + i] & mask_width(widths[kind]);
      if (got != expected) {
        fprintf(stderr,
                "%s to integer kind %zu differs at %zu: got 0x%016llx, "
                "expected 0x%016llx\n",
                name, kind, i, (unsigned long long)got,
                (unsigned long long)expected);
        return 1;
      }
    }
  }
  return 0;
}

#define CHECK_INTEGER_TO_FLOAT(name, array, function, signed_value)            \
  do {                                                                          \
    uint64_t output[count * 2];                                                  \
    long double values[count];                                                   \
    function((void *)(array), output);                                           \
    for (size_t index = 0; index < count; ++index)                              \
      values[index] = (long double)(array)[index];                              \
    failures += check_integer_float(name, output, values, signed_value);         \
  } while (0)

int main(void) {
  int failures = 0;
  CHECK_INTEGER_TO_FLOAT("i8", i8_values, fas_i8_float, 1);
  CHECK_INTEGER_TO_FLOAT("u8", u8_values, fas_u8_float, 0);
  CHECK_INTEGER_TO_FLOAT("i16", i16_values, fas_i16_float, 1);
  CHECK_INTEGER_TO_FLOAT("u16", u16_values, fas_u16_float, 0);
  CHECK_INTEGER_TO_FLOAT("i32", i32_values, fas_i32_float, 1);
  CHECK_INTEGER_TO_FLOAT("u32", u32_values, fas_u32_float, 0);
  CHECK_INTEGER_TO_FLOAT("i64", i64_values, fas_i64_float, 1);
  CHECK_INTEGER_TO_FLOAT("u64", u64_values, fas_u64_float, 0);
  CHECK_INTEGER_TO_FLOAT("isize", isize_values, fas_isize_float, 1);
  CHECK_INTEGER_TO_FLOAT("usize", usize_values, fas_usize_float, 0);

  uint64_t converted32[count * integer_kind_count];
  uint64_t converted64[count * integer_kind_count];
  fas_f32_int((void *)f32_values, converted32);
  fas_f64_int((void *)f64_values, converted64);
  double widened32[count];
  double widened64[count];
  for (size_t i = 0; i < count; ++i) {
    widened32[i] = f32_values[i];
    widened64[i] = f64_values[i];
  }
  failures += check_float_int("f32", widened32, converted32);
  failures += check_float_int("f64", widened64, converted64);

  uint64_t converted_float[count * 2];
  uint64_t converted_bits[count * 2];
  fas_float_float((void *)f32_values, (void *)f64_values, converted_float);
  fas_float_bits((void *)f32_values, (void *)f64_values, converted_bits);
  for (size_t i = 0; i < count; ++i) {
    uint64_t expected64 = bits64((double)f32_values[i]);
    uint32_t expected32 = bits32((float)f64_values[i]);
    uint32_t actual32 = (uint32_t)converted_float[count + i];
    if (isnan(f32_values[i]) ? !quiet_nan64(converted_float[i])
                             : converted_float[i] != expected64) {
      fprintf(stderr, "f32 to f64 differs at %zu\n", i);
      ++failures;
    }
    if (isnan(f64_values[i]) ? !quiet_nan32(actual32)
                             : actual32 != expected32) {
      fprintf(stderr, "f64 to f32 differs at %zu\n", i);
      ++failures;
    }
    if ((uint32_t)converted_bits[i] != bits32(f32_values[i]) ||
        converted_bits[count + i] != bits64(f64_values[i])) {
      fprintf(stderr, "float bitcast differs at %zu\n", i);
      ++failures;
    }
  }
  return failures == 0 ? 0 : 1;
}

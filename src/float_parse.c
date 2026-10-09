#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <locale.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static locale_t fas_c_locale;

static locale_t get_c_locale(void) {
  if (fas_c_locale == (locale_t)0) {
    fas_c_locale = newlocale(LC_NUMERIC_MASK, "C", (locale_t)0);
    if (fas_c_locale == (locale_t)0) caml_failwith("cannot create C numeric locale");
  }
  return fas_c_locale;
}

CAMLprim value fas_parse_f32_bits(value text) {
  float parsed = strtof_l(String_val(text), NULL, get_c_locale());
  uint32_t bits;
  memcpy(&bits, &parsed, sizeof(bits));
  return caml_copy_int64((int64_t)bits);
}

CAMLprim value fas_parse_f64_bits(value text) {
  double parsed = strtod_l(String_val(text), NULL, get_c_locale());
  uint64_t bits;
  memcpy(&bits, &parsed, sizeof(bits));
  return caml_copy_int64((int64_t)bits);
}

CAMLprim value fas_f32_binary_bits(value operation, value left_bits, value right_bits) {
  uint32_t left_raw = (uint32_t)Int64_val(left_bits);
  uint32_t right_raw = (uint32_t)Int64_val(right_bits);
  float left;
  float right;
  memcpy(&left, &left_raw, sizeof(left));
  memcpy(&right, &right_raw, sizeof(right));
  volatile float result;
  switch (Int_val(operation)) {
  case 0: result = left + right; break;
  case 1: result = left - right; break;
  case 2: result = left * right; break;
  default: result = left / right; break;
  }
  uint32_t result_bits;
  memcpy(&result_bits, (const void *)&result, sizeof(result_bits));
  return caml_copy_int64((int64_t)result_bits);
}

CAMLprim value fas_int_to_float_bits(value raw, value width, value is_signed, value to_f32) {
  int64_t signed_value = Int64_val(raw);
  uint64_t unsigned_value = (uint64_t)signed_value;
  unsigned bits = (unsigned)Int_val(width);
  uint64_t result_bits;
  if (Bool_val(is_signed)) {
    switch (bits) {
    case 8: signed_value = (int8_t)signed_value; break;
    case 16: signed_value = (int16_t)signed_value; break;
    case 32: signed_value = (int32_t)signed_value; break;
    }
    if (Bool_val(to_f32)) {
      float result = (float)signed_value;
      uint32_t narrowed;
      memcpy(&narrowed, &result, sizeof(narrowed));
      result_bits = narrowed;
    } else {
      double result = (double)signed_value;
      memcpy(&result_bits, &result, sizeof(result_bits));
    }
  } else {
    if (bits == 8) unsigned_value = (uint8_t)unsigned_value;
    else if (bits == 16) unsigned_value = (uint16_t)unsigned_value;
    else if (bits == 32) unsigned_value = (uint32_t)unsigned_value;
    if (Bool_val(to_f32)) {
      float result = (float)unsigned_value;
      uint32_t narrowed;
      memcpy(&narrowed, &result, sizeof(narrowed));
      result_bits = narrowed;
    } else {
      double result = (double)unsigned_value;
      memcpy(&result_bits, &result, sizeof(result_bits));
    }
  }
  return caml_copy_int64((int64_t)result_bits);
}

CAMLprim value fas_float_to_int_bits(value raw, value format, value width, value is_signed) {
  int64_t input_bits = Int64_val(raw);
  unsigned source_format = (unsigned)Int_val(format);
  unsigned bits = (unsigned)Int_val(width);
  long double input;
  if (source_format == 32) {
    uint32_t narrowed = (uint32_t)input_bits;
    float value;
    memcpy(&value, &narrowed, sizeof(value));
    input = value;
  } else {
    double value;
    memcpy(&value, &input_bits, sizeof(value));
    input = value;
  }
  uint64_t result;
  if (Bool_val(is_signed)) {
    int64_t minimum = bits == 64 ? INT64_MIN : -(INT64_C(1) << (bits - 1));
    int64_t maximum = bits == 64 ? INT64_MAX : (INT64_C(1) << (bits - 1)) - 1;
    if (isnan(input)) result = 0;
    else if (input <= (long double)minimum) result = (uint64_t)minimum;
    else if (input >= (long double)maximum) result = (uint64_t)maximum;
    else result = (uint64_t)(int64_t)input;
  } else {
    uint64_t maximum = bits == 64 ? UINT64_MAX : (UINT64_C(1) << bits) - 1;
    if (isnan(input) || input <= 0.0L) result = 0;
    else if (input >= (long double)maximum) result = maximum;
    else result = (uint64_t)input;
  }
  if (bits < 64) result &= (UINT64_C(1) << bits) - 1;
  int64_t boxed;
  memcpy(&boxed, &result, sizeof(boxed));
  return caml_copy_int64(boxed);
}

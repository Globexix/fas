#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

static double input64(uint64_t bits) {
  double x;
  memcpy(&x, &bits, 8);
  return x;
}
static float input32(uint32_t bits) {
  float x;
  memcpy(&x, &bits, 4);
  return x;
}
static uint64_t bits64(double x) {
  uint64_t bits;
  memcpy(&bits, &x, 8);
  return bits;
}
static uint64_t bits32(float x) {
  uint32_t bits;
  memcpy(&bits, &x, 4);
  return bits;
}

CAMLprim value proof_unary(value format, value operation, value raw) {
  int op = Int_val(operation);
  uint64_t bits = Int64_val(raw), result;
  if (Bool_val(format)) {
    float a = input32(bits);
    switch (op) {
    case 0: result = bits32(sqrtf(a)); break;
    case 1: result = bits32(floorf(a)); break;
    case 2: result = bits32(ceilf(a)); break;
    case 3: result = bits32(roundevenf(a)); break;
    case 4: result = bits32(fabsf(a)); break;
    case 5: result = bits32(-a); break;
    default: result = bits64((double)a); break;
    }
  } else {
    double a = input64(bits);
    switch (op) {
    case 0: result = bits64(sqrt(a)); break;
    case 1: result = bits64(floor(a)); break;
    case 2: result = bits64(ceil(a)); break;
    case 3: result = bits64(roundeven(a)); break;
    case 4: result = bits64(fabs(a)); break;
    case 5: result = bits64(-a); break;
    default: result = bits32((float)a); break;
    }
  }
  return caml_copy_int64(result);
}

CAMLprim value proof_binary(value format, value operation, value left, value right) {
  uint64_t a_bits = Int64_val(left), b_bits = Int64_val(right), result;
  int op = Int_val(operation);
  if (Bool_val(format)) {
    float a = input32(a_bits), b = input32(b_bits);
    switch (op) {
    case 0: result = bits32(a + b); break;
    case 1: result = bits32(a - b); break;
    case 2: result = bits32(a * b); break;
    case 3: result = bits32(a / b); break;
    case 4: result = a == b; break;
    case 5: result = a != b; break;
    case 6: result = a < b; break;
    case 7: result = a <= b; break;
    case 8: result = a > b; break;
    default: result = a >= b; break;
    }
  } else {
    double a = input64(a_bits), b = input64(b_bits);
    switch (op) {
    case 0: result = bits64(a + b); break;
    case 1: result = bits64(a - b); break;
    case 2: result = bits64(a * b); break;
    case 3: result = bits64(a / b); break;
    case 4: result = a == b; break;
    case 5: result = a != b; break;
    case 6: result = a < b; break;
    case 7: result = a <= b; break;
    case 8: result = a > b; break;
    default: result = a >= b; break;
    }
  }
  return caml_copy_int64(result);
}

CAMLprim value proof_fma(value format, value a, value b, value c) {
  uint64_t result = Bool_val(format)
    ? bits32(fmaf(input32(Int64_val(a)), input32(Int64_val(b)), input32(Int64_val(c))))
    : bits64(fma(input64(Int64_val(a)), input64(Int64_val(b)), input64(Int64_val(c))));
  return caml_copy_int64(result);
}

CAMLprim value proof_to_int(value format, value width, value sign, value raw) {
  long double x = Bool_val(format) ? (long double)input32(Int64_val(raw))
                                 : (long double)input64(Int64_val(raw));
  int n = Int_val(width);
  long double bound = ldexpl(1.0L, Bool_val(sign) ? n - 1 : n);
  uint64_t result;
  if (isnan(x)) result = 0;
  else if (Bool_val(sign)) {
    if (x < -bound) result = UINT64_C(1) << (n - 1);
    else if (x >= bound) result = (UINT64_C(1) << (n - 1)) - 1;
    else result = (uint64_t)(int64_t)truncl(x);
  } else {
    if (x <= 0) result = 0;
    else if (x >= bound) result = n == 64 ? UINT64_MAX : (UINT64_C(1) << n) - 1;
    else result = (uint64_t)truncl(x);
  }
  if (n < 64) result &= (UINT64_C(1) << n) - 1;
  return caml_copy_int64(result);
}

CAMLprim value proof_from_int(value format, value width, value sign, value raw) {
  uint64_t u = Int64_val(raw);
  int n = Int_val(width);
  if (n < 64) u &= (UINT64_C(1) << n) - 1;
  int64_t s;
  if (n < 64 && (u & (UINT64_C(1) << (n - 1)))) u |= ~((UINT64_C(1) << n) - 1);
  memcpy(&s, &u, 8);
  if (!Bool_val(sign) && n < 64) u &= (UINT64_C(1) << n) - 1;
  uint64_t result;
  if (Bool_val(format)) result = bits32(Bool_val(sign) ? (float)s : (float)u);
  else result = bits64(Bool_val(sign) ? (double)s : (double)u);
  return caml_copy_int64(result);
}

#include <caml/memory.h>
#include <stdio.h>
#include <stdlib.h>

static void exact_decimal(char *out, long double x, int f32) {
  snprintf(out, 1800, "%.*Le", f32 ? 112 : 800, x);
  char *exponent = strchr(out, 'e');
  char *last = exponent - 1;
  while (*last == '0' && last[-1] != '.') --last;
  memmove(last + 1, exponent, strlen(exponent) + 1);
}
static void last_digit(char *out, const char *input, int direction) {
  strcpy(out, input);
  char *last = strchr(out, 'e') - 1;
  while (last >= out && (*last == '.' || *last == '-' || *last == '+')) --last;
  while (last >= out) {
    if (*last == '.') { --last; continue; }
    if (direction > 0 && *last == '9') { *last-- = '0'; continue; }
    if (direction < 0 && *last == '0') { *last-- = '9'; continue; }
    *last += direction;
    break;
  }
}
CAMLprim value proof_literals(value format, value raw) {
  CAMLparam2(format, raw);
  CAMLlocal3(result, pair, text);
  int f32 = Bool_val(format);
  uint64_t bits = Int64_val(raw);
  long double a = f32 ? (long double)input32(bits) : (long double)input64(bits);
  char cases[6][1800];
  int max_digits = f32 ? 9 : 17;
  for (int n = 1; n <= max_digits; ++n) {
    snprintf(cases[0], 1800, "%.*Lg", n, a);
    uint64_t parsed = f32 ? bits32(strtof(cases[0], NULL)) : bits64(strtod(cases[0], NULL));
    if (parsed == bits) break;
  }
  snprintf(cases[1], 1800, "%.*Lg", max_digits, a);
  for (int i = 0; i < 2; ++i)
    if (!strchr(cases[i], '.') && !strchr(cases[i], 'e')) strcat(cases[i], ".0");
  exact_decimal(cases[2], a, f32);
  uint64_t adjacent = bits + 1;
  long double b = f32 ? (long double)input32(adjacent) : (long double)input64(adjacent);
  int count = isfinite(b) ? 6 : 3;
  if (count == 6) {
    long double midpoint = (a + b) / 2;
    exact_decimal(cases[3], midpoint, f32);
    last_digit(cases[4], cases[3], -1);
    last_digit(cases[5], cases[3], 1);
  }
  result = caml_alloc(count, 0);
  for (int i = 0; i < count; ++i) {
    pair = caml_alloc_tuple(2);
    text = caml_copy_string(cases[i]);
    Store_field(pair, 0, text);
    uint64_t expected = f32 ? bits32(strtof(cases[i], NULL)) : bits64(strtod(cases[i], NULL));
    Store_field(pair, 1, caml_copy_int64(expected));
    Store_field(result, i, pair);
  }
  CAMLreturn(result);
}

#include <dlfcn.h>
#include <caml/fail.h>
static void *proof_library;
CAMLprim value proof_load(value path) {
  if (proof_library) dlclose(proof_library);
  proof_library = dlopen(String_val(path), RTLD_NOW | RTLD_LOCAL);
  if (!proof_library) caml_failwith(dlerror());
  return Val_unit;
}
CAMLprim value proof_runtime(value shape, value a, value b, value c) {
  CAMLparam4(shape, a, b, c);
  CAMLlocal1(result);
  int encoded = Int_val(shape), constant = encoded <= -1000;
  if (constant) encoded = -1000 - encoded;
  int f32 = encoded < 100, shape_index = encoded % 100;
  int lanes = shape_index ? shape_index : 1;
  char symbol[64];
  snprintf(symbol, sizeof(symbol), "proof%s%d_%d", constant ? "_const" : "", f32 ? 32 : 64, shape_index);
  void (*run)(const void *, const void *, const void *, void *) = dlsym(proof_library, symbol);
  if (!run) caml_failwith(dlerror());
  uint64_t output[64 * 48] = {0}, av[64] = {0}, bv[64] = {0}, cv[64] = {0};
  uint32_t as[64] = {0}, bs[64] = {0}, cs[64] = {0};
  for (int i = 0; i < lanes; ++i) {
    av[i] = Int64_val(Field(a, i)); bv[i] = Int64_val(Field(b, i)); cv[i] = Int64_val(Field(c, i));
    as[i] = av[i]; bs[i] = bv[i]; cs[i] = cv[i];
  }
  run(f32 ? (const void *)as : (const void *)av,
      f32 ? (const void *)bs : (const void *)bv,
      f32 ? (const void *)cs : (const void *)cv, output);
  int operations = 17 + (lanes <= 32 ? 1 : 0);
  for (int width = 8; width <= 64; width *= 2) if (lanes * width <= 2048) operations += 4;
  if (lanes <= 32) operations += 4;
  result = caml_alloc(lanes * operations, 0);
  for (int i = 0; i < lanes * operations; ++i) Store_field(result, i, caml_copy_int64(output[i]));
  CAMLreturn(result);
}

CAMLprim value proof_boundaries(value format) {
  CAMLparam1(format);
  CAMLlocal3(result, pair, text);
  int f32 = Bool_val(format);
  long double limits[2] = {
    ldexpl(1.0L, f32 ? 128 : 1024) - ldexpl(1.0L, f32 ? 103 : 970),
    ldexpl(1.0L, f32 ? -150 : -1075)
  };
  result = caml_alloc(6, 0);
  for (int i = 0; i < 2; ++i) {
    char exact[1800], adjusted[1800];
    exact_decimal(exact, limits[i], f32);
    for (int j = 0; j < 3; ++j) {
      if (j == 1) strcpy(adjusted, exact);
      else last_digit(adjusted, exact, j == 0 ? -1 : 1);
      pair = caml_alloc_tuple(2);
      text = caml_copy_string(adjusted);
      Store_field(pair, 0, text);
      uint64_t bits = f32 ? bits32(strtof(adjusted, NULL)) : bits64(strtod(adjusted, NULL));
      Store_field(pair, 1, caml_copy_int64(bits));
      Store_field(result, i * 3 + j, pair);
    }
  }
  CAMLreturn(result);
}

CAMLprim value proof_parse_literal(value format, value text) {
  uint64_t bits = Bool_val(format) ? bits32(strtof(String_val(text), NULL))
                                 : bits64(strtod(String_val(text), NULL));
  return caml_copy_int64(bits);
}

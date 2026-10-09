#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <locale.h>
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

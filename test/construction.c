#include <stdint.h>
#include <stdio.h>

extern int32_t fas_construct_order(void);
extern int32_t fas_construct_nested_order(void);
extern uint32_t fas_construct_array(void);
extern int32_t fas_construct_explicit(void);
extern uint32_t fas_construct_shuffle(void);
extern uint32_t fas_construct_contextual_vector(void);
extern uint32_t fas_construct_vector_in_struct(void);
extern uint32_t fas_construct_array_of_vectors(void);

static int32_t recorded[16];
static int32_t recorded_count;

int32_t fas_record(int32_t value) {
  recorded[recorded_count++] = value;
  return value;
}

static int recorded_values(const int32_t *expected, int32_t count) {
  if (recorded_count != count) return 0;
  for (int32_t i = 0; i < count; ++i) {
    if (recorded[i] != expected[i]) return 0;
  }
  return 1;
}

int main(void) {
  const int32_t pair_order[] = {1, 2};
  recorded_count = 0;
  if (fas_construct_order() != 12 || !recorded_values(pair_order, 2)) {
    fputs("struct construction order or value failed\n", stderr);
    return 1;
  }

  const int32_t nested_order[] = {3, 4, 5, 6};
  recorded_count = 0;
  if (fas_construct_nested_order() != 6 || !recorded_values(nested_order, 4)) {
    fputs("nested construction order or value failed\n", stderr);
    return 2;
  }

  if (fas_construct_array() != 10) {
    fputs("array construction failed\n", stderr);
    return 3;
  }
  if (fas_construct_explicit() != 28) {
    fputs("explicit struct construction failed\n", stderr);
    return 4;
  }
  if (fas_construct_shuffle() != 20481) {
    fputs("vector literal shuffle selector failed\n", stderr);
    return 5;
  }
  if (fas_construct_contextual_vector() != 2357u) {
    fputs("contextual vector construction failed\n", stderr);
    return 6;
  }
  if (fas_construct_vector_in_struct() != 1113u) {
    fputs("vector-in-struct construction failed\n", stderr);
    return 7;
  }
  if (fas_construct_array_of_vectors() != 88u) {
    fputs("array-of-vectors construction failed\n", stderr);
    return 8;
  }
  return 0;
}

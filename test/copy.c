#include <stdint.h>
#include <stdio.h>
#include <string.h>

struct Cell {
  uint32_t x;
  uint32_t y;
};

struct Nested {
  struct Cell cells[2];
  uint64_t tail;
};

extern void fas_copy_nested(void *dst, void *src);
extern void fas_copy_flag(void *dst, void *src);
extern void fas_copy_self(void *value);
extern void fas_copy_right(void *dst, void *src);
extern void fas_copy_left(void *dst, void *src);
extern void fas_copy_loop(void *dst, void *src, uint32_t count);
extern void fas_copy_order(void *dst, void *src);
extern void fas_copy_empty_order(void *dst, void *src);
extern void fas_copy_constant(void *dst);
extern void fas_copy_views(void *dst, void *src);

static int32_t order_values[8];
static int32_t order_count;

int32_t fas_record(int32_t value) {
  if (order_count < 8) order_values[order_count++] = value;
  return 0;
}

static int check_order(int32_t first, int32_t second) {
  return order_count == 2 && order_values[0] == first && order_values[1] == second;
}

int main(void) {
  struct Nested source = {{{11, 12}, {21, 22}}, UINT64_C(0x123456789abcdef0)};
  struct Nested destination = {0};
  fas_copy_nested(&destination, &source);
  if (destination.cells[0].x != 11 || destination.cells[0].y != 12 ||
      destination.cells[1].x != 21 || destination.cells[1].y != 22 ||
      destination.tail != source.tail) {
    fputs("nested struct copy failed\n", stderr);
    return 1;
  }

  unsigned char flag_source[8] = {0};
  unsigned char flag_destination[8];
  uint32_t tag = UINT32_C(0xa1b2c3d4);
  memset(flag_source, 0x5a, sizeof(flag_source));
  memset(flag_destination, 0xa5, sizeof(flag_destination));
  flag_source[0] = 2;
  memcpy(flag_source + 4, &tag, sizeof(tag));
  fas_copy_flag(flag_destination, flag_source);
  uint32_t copied_tag = 0;
  memcpy(&copied_tag, flag_destination + 4, sizeof(copied_tag));
  if (flag_destination[0] != 1 || copied_tag != tag) {
    fputs("bool leaf was not normalized during copy\n", stderr);
    return 2;
  }

  uint32_t self[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  uint32_t self_before[8];
  memcpy(self_before, self, sizeof(self));
  fas_copy_self(self);
  if (memcmp(self, self_before, sizeof(self)) != 0) {
    fputs("self copy failed\n", stderr);
    return 3;
  }

  uint32_t overlap[9];
  uint32_t expected[9];
  for (uint32_t i = 0; i < 9; ++i) overlap[i] = expected[i] = i + 10;
  memmove(expected + 1, expected, 8 * sizeof(uint32_t));
  fas_copy_right(overlap + 1, overlap);
  if (memcmp(overlap, expected, sizeof(overlap)) != 0) {
    fputs("overlapping copy to higher address failed\n", stderr);
    return 4;
  }
  for (uint32_t i = 0; i < 9; ++i) overlap[i] = expected[i] = i + 30;
  memmove(expected, expected + 1, 8 * sizeof(uint32_t));
  fas_copy_left(overlap, overlap + 1);
  if (memcmp(overlap, expected, sizeof(overlap)) != 0) {
    fputs("overlapping copy to lower address failed\n", stderr);
    return 5;
  }

  uint32_t loop_source[8] = {101, 102, 103, 104, 105, 106, 107, 108};
  uint32_t loop_destination[8] = {0};
  fas_copy_loop(loop_destination, loop_source, 4);
  if (memcmp(loop_destination, loop_source, sizeof(loop_source)) != 0) {
    fputs("loop copy failed\n", stderr);
    return 6;
  }

  uint32_t order_source[2] = {51, 52};
  uint32_t order_destination[2] = {0};
  order_count = 0;
  fas_copy_order(order_destination, order_source);
  if (!check_order(0, 1) || order_destination[0] != 51 || order_destination[1] != 52) {
    fputs("copy operand evaluation order failed\n", stderr);
    return 7;
  }
  unsigned char empty_storage = 0;
  order_count = 0;
  fas_copy_empty_order(&empty_storage, &empty_storage);
  if (!check_order(2, 3)) {
    fputs("empty copy skipped operand evaluation\n", stderr);
    return 8;
  }

  uint32_t constant_destination[2] = {0};
  fas_copy_constant(constant_destination);
  if (constant_destination[0] != 31 || constant_destination[1] != 37) {
    fputs("constant array source copy failed\n", stderr);
    return 9;
  }

  struct Nested view_source = {{{61, 62}, {71, 72}}, UINT64_C(0xfedcba9876543210)};
  struct Nested view_destination = {0};
  fas_copy_views(&view_destination, &view_source);
  if (view_destination.cells[0].x != 61 || view_destination.cells[0].y != 62 ||
      view_destination.cells[1].x != 71 || view_destination.cells[1].y != 72 ||
      view_destination.tail != view_source.tail) {
    fputs("view copy failed\n", stderr);
    return 10;
  }

  return 0;
}

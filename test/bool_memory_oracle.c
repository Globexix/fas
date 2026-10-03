#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

struct ForeignBool {
  _Bool value;
};

extern uint32_t bool_memory_paths(void *, struct ForeignBool *, unsigned char *, volatile unsigned char *, _Bool);

int main(void) {
  unsigned char raw[2] = {2, 0};
  struct ForeignBool foreign = {0};
  ((unsigned char *)&foreign)[offsetof(struct ForeignBool, value)] = 2;
  unsigned char output[13];
  for (size_t i = 0; i < sizeof output; ++i) output[i] = 0xaa;
  volatile unsigned char volatile_output[2] = {0xaa, 0xaa};
  uint32_t observed = bool_memory_paths(raw, &foreign, output, volatile_output, 1);
  uint32_t expected = 8;
  printf("expected=%u observed=%u\n", expected, observed);
  const unsigned char expected_bytes[13] = {1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0};
  for (size_t i = 0; i < sizeof output; ++i) {
    if (output[i] != expected_bytes[i] || output[i] > 1) return 1;
  }
  return observed == expected && foreign.value == 1 && raw[0] == 1 && volatile_output[0] == 0 && volatile_output[1] == 1 ? 0 : 1;
}

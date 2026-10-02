#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

struct ForeignBool {
  _Bool value;
};

extern uint32_t bool_memory_paths(void *, struct ForeignBool *);

int main(void) {
  unsigned char raw[2] = {2, 0};
  struct ForeignBool foreign = {0};
  ((unsigned char *)&foreign)[offsetof(struct ForeignBool, value)] = 2;
  uint32_t observed = bool_memory_paths(raw, &foreign);
  uint32_t expected = 8;
  printf("expected=%u observed=%u\n", expected, observed);
  return observed == expected ? 0 : 1;
}

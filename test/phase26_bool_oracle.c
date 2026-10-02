#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

struct Phase26ForeignBool {
  _Bool value;
};

extern uint32_t phase26_bool_paths(void *, struct Phase26ForeignBool *);

int main(void) {
  unsigned char raw[2] = {2, 0};
  struct Phase26ForeignBool foreign = {0};
  ((unsigned char *)&foreign)[offsetof(struct Phase26ForeignBool, value)] = 2;
  uint32_t observed = phase26_bool_paths(raw, &foreign);
  uint32_t expected = 8;
  printf("expected=%u observed=%u\n", expected, observed);
  return observed == expected ? 0 : 1;
}

#include <stdio.h>
#define C_BLOCK_INCLUDE_IMPLEMENTATION
#include "c_block_include.h"
int main(void) {
  printf("%d %d\n", cbi_count("{\"a\":{\"b\":[1,{}]}}", '{'), cbi_count("[[]]", '['));
  return 0;
}

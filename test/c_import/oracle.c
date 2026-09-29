#include "runtime.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  if (strlen("fas") != 3) return 1;
  if (memcmp("abc", "abc", 3) != 0) return 2;
  printf("c-import:%d:%lu\n", -7, 42UL);
  if (runtime_enum_echo(RUNTIME_KIND) != RUNTIME_KIND) return 3;
  runtime_global += 2;
  if (runtime_global != 5) return 4;
  if (runtime_global_increment(3) != 8) return 5;
  int *slot = NULL;
  runtime_pointer_out(&slot);
  if (*slot != 41) return 6;
  struct RuntimeOpaque *object = runtime_opaque_value();
  if (runtime_opaque_roundtrip(object) != object) return 7;
  unsigned char *memory = malloc(32);
  if (memory == NULL) return 8;
  memory[0] = 173;
  if (memory[0] != 173) return 9;
  free(memory);
  return 0;
}

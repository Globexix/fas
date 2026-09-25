#include <stdio.h>

int observe(int value) {
  fprintf(stderr, "note %d\n", value);
  fflush(stderr);
  return value;
}

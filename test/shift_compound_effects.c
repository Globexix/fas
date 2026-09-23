#include <stdio.h>
#include <stddef.h>
#include <stdint.h>

static unsigned index_calls;
static unsigned count_calls;

void note(int value) {
  fprintf(stderr, "note %d\n", value);
  fflush(stderr);
}

size_t next_index(void) {
  index_calls += 1;
  fprintf(stderr, "index %u\n", index_calls);
  fflush(stderr);
  return 0;
}

uint32_t next_count(void) {
  count_calls += 1;
  fprintf(stderr, "count %u\n", count_calls);
  fflush(stderr);
  return count_calls;
}

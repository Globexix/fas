#include <stdint.h>

typedef struct {
  uint32_t value;
} Inner;

typedef struct {
  uint32_t id;
  Inner inner;
} Pair;

static uint32_t values[] = {10, 20, 30, 40};
static Pair pairs[] = {{1, {2}}, {3, {4}}};
static uint32_t calls;

uint32_t *buffer(void) {
  ++calls;
  return values;
}

Pair *pair_buffer(void) {
  return pairs;
}

uint32_t pointer_calls(void) {
  return calls;
}

void set_pair(Pair *p, uint32_t value) {
  p->id = value;
}

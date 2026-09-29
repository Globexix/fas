#include <stdint.h>

struct ViewInner {
  uint32_t value;
};

struct ViewState {
  uint32_t count;
  struct ViewInner inner;
};

static struct ViewState state = {10, {20}};

void *view_state(void) {
  return &state;
}

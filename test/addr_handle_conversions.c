#include <stdint.h>
#include <stddef.h>

struct O {
  uint64_t value;
};

static struct O object = {42};

uint64_t observe_addr(void *p) {
  return (uint64_t)(uintptr_t)p;
}

void *c_null_addr(void) {
  return NULL;
}

void *c_nonnull_addr(void) {
  return &object;
}

struct O *c_null_handle(void) {
  return NULL;
}

struct O *c_nonnull_handle(void) {
  return &object;
}

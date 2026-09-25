#include <stdint.h>

uint64_t observe_addr(void *p) {
  return (uint64_t)(uintptr_t)p;
}

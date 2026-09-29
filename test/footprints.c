#define _GNU_SOURCE
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

void *footprint_buffer(void);
void fas_write_footprints(void);
uint32_t fas_load_bool_footprint(void);
void fas_guard_store(void *p);
int32_t fas_guard_load(void *p);
void fas_guard_bool_store(void *p);
int32_t fas_guard_bool_load(void *p);

static uint8_t buffer[128];

void *footprint_buffer(void) { return buffer; }

static int untouched(void) {
  for (size_t i = 0; i < sizeof(buffer); ++i) {
    if ((i >= 4 && i < 8) || (i >= 20 && i < 32) || (i >= 40 && i < 42) ||
        (i >= 60 && i < 65) || (i >= 84 && i < 88))
      continue;
    if (buffer[i] != 0xa5)
      return 0;
  }
  return 1;
}

int main(void) {
  memset(buffer, 0xa5, sizeof(buffer));
  fas_write_footprints();

  if (!untouched() || memcmp(buffer + 4, "\x44\x33\x22\x11", 4))
    return 1;
  if (buffer[20] != 1 || buffer[24] != 2 || buffer[28] != 3)
    return 2;
  if (buffer[40] != 0xa5 || buffer[41] != 0x09)
    return 3;
  if (buffer[60] != 11 || buffer[61] != 22 ||
      buffer[62] != 33 || buffer[63] != 44 || buffer[64] != 55)
    return 4;
  if (memcmp(buffer + 84, "\x88\x77\x66\x55", 4))
    return 5;

  buffer[40] = 0xa5;
  buffer[41] = 0xfa;
  if (fas_load_bool_footprint() != 0x0aa5)
    return 6;

  long page_size = sysconf(_SC_PAGESIZE);
  if (page_size <= 0)
    return 7;
  uint8_t *mapping = mmap(NULL, (size_t)page_size * 2, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mapping == MAP_FAILED)
    return 8;
  if (mprotect(mapping + page_size, (size_t)page_size, PROT_NONE) != 0)
    return 9;
  uint32_t *last = (uint32_t *)(mapping + page_size - 12);
  fas_guard_store(last);
  if (last[0] != UINT32_C(305419896) || last[1] != UINT32_C(2271560481) ||
      last[2] != UINT32_C(2309737967))
    return 10;
  if (fas_guard_load(last) != 0)
    return 11;
  uint8_t *last_bool = mapping + page_size - 3;
  fas_guard_bool_store(last_bool);
  if (last_bool[0] != 0xa5 || last_bool[1] != 0x55 || last_bool[2] != 0x01)
    return 12;
  if (fas_guard_bool_load(last_bool) != 0)
    return 13;
  if (munmap(mapping, (size_t)page_size * 2) != 0)
    return 14;
  return 0;
}

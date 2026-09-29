#define _GNU_SOURCE
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

uint8_t *volatile_vector_buffer(void);
int32_t fas_volatile_run(void);
void fas_volatile_loop(void *p, uint32_t count);
int32_t fas_volatile_guard(void *p);

static uint8_t buffer[96];

uint8_t *volatile_vector_buffer(void) { return buffer; }

int main(void) {
  memset(buffer, 0xa5, sizeof(buffer));
  buffer[32] = 0xa5;
  buffer[33] = 0xfa;
  if (fas_volatile_run() != 0)
    return 1;

  uint32_t values[3];
  memcpy(values, buffer, sizeof(values));
  if (values[0] != UINT32_C(287454020) || values[1] != UINT32_C(1432778632) ||
      values[2] != UINT32_C(2578103244))
    return 2;
  if (buffer[12] != 0xa5 || buffer[31] != 0xa5 || buffer[34] != 0xa5 ||
      buffer[39] != 0xa5 || buffer[40] != 0xa5 || buffer[41] != 0x09 ||
      buffer[42] != 0xa5)
    return 3;

  fas_volatile_loop(buffer + 64, 5);
  memcpy(values, buffer + 64, sizeof(values));
  if (values[0] != 4 || values[1] != 4 || values[2] != 4)
    return 4;
  if (buffer[63] != 0xa5 || buffer[76] != 0xa5)
    return 5;

  long page_size = sysconf(_SC_PAGESIZE);
  if (page_size <= 0)
    return 6;
  uint8_t *mapping = mmap(NULL, (size_t)page_size * 2, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mapping == MAP_FAILED)
    return 7;
  if (mprotect(mapping + page_size, (size_t)page_size, PROT_NONE) != 0)
    return 8;
  if (fas_volatile_guard(mapping + page_size - 12) != 0)
    return 9;
  if (munmap(mapping, (size_t)page_size * 2) != 0)
    return 10;
  return 0;
}

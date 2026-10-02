#include <stddef.h>
#include <string.h>

static int fas_abs(int value) { return value + 1; }

static unsigned char fas_byte_after_memset(unsigned char value) {
  (void)value;
  return 5;
}

static size_t fas_strlen(const char *text) {
  (void)text;
  return 7;
}

static void *fas_malloc(size_t size) {
  (void)size;
  return NULL;
}

int main(void) {
  unsigned char byte = 5;
  if (fas_abs(0) != 1) return 1;
  byte = fas_byte_after_memset(byte);
  if (byte != 5) return 3;
  if (fas_strlen("abc") != 7) return 4;
  if (fas_malloc(8) != NULL) return 5;
  if (strlen("abc") != 3) return 6;
  return 0;
}

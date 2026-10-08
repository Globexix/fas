#include <stdbool.h>
#include <stdint.h>

char *offset_if(char *, bool, int8_t, int8_t);
char *offset_plain(char *, int8_t);
char *offset_scaled_if(char *, bool, uint8_t);
char *offset_scaled_plain(char *, uint8_t);
char *offset_compound_if(char *, bool, int8_t, int8_t);

int main(void) {
  char bytes[256] = {0};
  char *base = &bytes[3];
  if (offset_if(base, true, -1, 2) != &bytes[2]) return 1;
  if (offset_if(base, false, -1, 2) != &bytes[5]) return 2;
  if (offset_plain(base, -1) != &bytes[2]) return 3;
  if (offset_compound_if(base, true, -1, 2) != &bytes[2]) return 4;
  if (offset_compound_if(base, false, -1, 2) != &bytes[5]) return 5;
  if (offset_scaled_if(bytes, true, 2) != &bytes[(uint8_t)(2 * 200)]) return 6;
  if (offset_scaled_plain(bytes, 2) != &bytes[(uint8_t)(2 * 200)]) return 7;
  return 0;
}

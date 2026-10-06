#include <stdint.h>
#include <stdlib.h>

int main(void) {
  uint32_t *p = malloc(16);
  if (p == NULL) return 1;
  p[3] = UINT32_C(0x12345678);
  if (p[3] != UINT32_C(0x12345678)) return 2;
  free(p);

  uint32_t *q = calloc(4, 4);
  if (q == NULL) return 3;
  q[0] = 11;
  q[3] = 29;
  if (q[0] != 11 || q[3] != 29) return 4;
  free(q);

  uint32_t *r = malloc(16);
  if (r == NULL) return 5;
  r = realloc(r, 32);
  if (r == NULL) return 6;
  r[7] = 71;
  if (r[7] != 71) return 7;
  free(r);
  return 0;
}

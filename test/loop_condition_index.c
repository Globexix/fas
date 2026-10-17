#include <stdint.h>
#include <stdio.h>

static const int32_t T[5] = {10, 20, 30, 40, 50};
static const int32_t G[5] = {3, 1, 4, 1, 5};
static const int32_t LANES[4] = {10, 20, 30, 40};

struct P {
  int32_t x;
  int32_t y;
  int32_t z;
};

static struct P POS[8];

int main(void) {
  int32_t k = 0;
  while (T[k] != 40) {
    k += 1;
    if (k > 4) {
      break;
    }
  }
  printf("while-const %d\n", k);

  uint64_t g = 0;
  while (G[g] != 5) {
    g += 1;
  }
  printf("while-global %lu\n", (unsigned long)g);

  int32_t loc[5] = {7, 8, 9, 10, 11};
  int32_t j = 0;
  while (loc[j] != 10) {
    j += 1;
  }
  printf("while-local %d\n", j);

  uint64_t u = 0;
  while (T[u] != 30) {
    u += 1;
  }
  printf("while-usize %lu\n", (unsigned long)u);

  int32_t s = 0;
  while (T[s + 0] != 50) {
    s += 1;
  }
  printf("while-plus-zero %d\n", s);

  int32_t hits = 0;
  for (int32_t f = 0; f < 5 && T[f] != 40; f += 1) {
    hits += 1;
  }
  printf("for-cond %d\n", hits);

  uint64_t n = 5;
  uint64_t m = 0;
  while (m < n && T[m] != 30) {
    m += 1;
  }
  printf("bound-match %lu\n", (unsigned long)m);

  uint64_t b = 0;
  while (b < n && T[b] != 99) {
    b += 1;
  }
  printf("bound-miss %lu\n", (unsigned long)b);

  int32_t i = 0;
  while (i < 3) {
    int32_t p = 0;
    while (p < 5 && T[p] != 20 * (i + 1)) {
      p += 1;
    }
    printf("nested %d\n", p);
    i += 1;
  }

  int32_t k2 = 0;
  int32_t w = 0;
  while (k2 < 5 && T[k2] != 50) {
    k2 += 1;
    if (k2 == 2) {
      continue;
    }
    w += 1;
  }
  printf("while-continue %d %d\n", k2, w);

  int32_t c = 0;
  for (int32_t q = 0; q < 5 && T[q] != 40; q += 1) {
    if (q == 1) {
      continue;
    }
    c += 1;
  }
  printf("for-continue %d\n", c);

  int32_t acc = 0;
  for (uint64_t x = 0; x < 5; x += 1) {
    acc += T[x];
  }
  printf("for-induction %d\n", acc);

  int32_t acc2 = 0;
  for (uint64_t y = 0; y < 5 && T[y] != 40; y += 1) {
    acc2 += T[y];
  }
  printf("for-induction-cond %d\n", acc2);

  int32_t one = 0;
  for (uint64_t z = 0; z < 1; z += 1) {
    one += T[z];
  }
  printf("single-iteration %d\n", one);

  int32_t once = 0;
  for (int32_t r = 2; r < 3 && T[r] == 30; r += 1) {
    once += 1;
  }
  printf("single-cond %d\n", once);

  uint64_t e = 0;
  while (e < 4 && LANES[e] != 30) {
    e += 1;
  }
  printf("lane-match %lu\n", (unsigned long)e);

  uint64_t f2 = 0;
  while (f2 < 4 && LANES[f2] != 99) {
    f2 += 1;
  }
  printf("lane-miss %lu\n", (unsigned long)f2);

  int32_t lsum = 0;
  for (uint64_t li = 0; li < 4; li += 1) {
    lsum += LANES[li];
  }
  printf("lane-sum %d\n", lsum);

  int32_t channel = G[0];
  if (channel < 1) {
    return 1;
  }
  struct P *row = &POS[channel - 1];
  row->x = 5;
  struct P *tail = &POS[2];
  printf("view-range %d\n", tail->x);

  return 0;
}

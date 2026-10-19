#include <stdint.h>
#include <stdio.h>

static uint32_t mix4(uint32_t x) {
  for (uint32_t i = 0; i < 4; i++) x = x * 31 + i;
  return x;
}

static uint32_t skip4(uint32_t x) {
  for (uint32_t i = 0; i < 4; i++) {
    if (x == i) continue;
    x += i * 7;
  }
  return x;
}

static uint32_t scale(uint32_t k, uint32_t x) { return x * k + k; }

static size_t sizes(void) {
  size_t total = 0;
  for (size_t i = 0; i < 5; i++) total += (i + 1) * (i + 3);
  return total;
}

static uint32_t pruned(uint32_t x) {
  for (uint32_t i = 0; i < 6; i++) {
    if (i == 0) x += 1000;
    else if (i % 2 == 0) x = scale(i, x);
    else x ^= i << 4;
  }
  return x;
}

static uint64_t triangle(uint64_t x) {
  for (uint64_t i = 0; i < 5; i++)
    for (uint64_t j = i; j < 5; j++) {
      if (j == 3) continue;
      x = x * 3 + i * 10 + j;
    }
  return x;
}

static int64_t signed_range(int64_t x) {
  for (int64_t i = -3; i < 3; i++) x = x * 2 + i;
  return x;
}

static int32_t narrow(int32_t x, uint8_t y) {
  for (int32_t i = -3; i < 2; i++) x = x * 3 + i;
  for (unsigned i = 250; i < 255; i++) y ^= (uint8_t)i;
  for (int i = -128; i < -125; i++) x += i;
  return x + y;
}

static uint64_t window(size_t n, const uint64_t *values) {
  uint64_t total = 0;
  for (size_t i = 0; i < n; i++)
    for (size_t j = i; j < n; j++) total += values[j] << i;
  return total;
}

static uint32_t first_match(uint32_t x) {
  for (uint32_t i = 0; i < 8; i++)
    if ((x & 7) == i) return scale(i + 1, x);
  return 0;
}

static uint32_t labeled(uint32_t limit) {
  uint32_t total = 0, n = 0;
  while (n < limit) {
    n += 1;
    for (uint32_t i = 0; i < 4; i++) {
      if (n % 3 == i) goto next;
      if (n == 11) goto done;
      total += n * (i + 1);
    }
  next:;
  }
done:
  return total;
}

static uint32_t deferred(uint32_t x) {
  for (uint32_t i = 0; i < 3; i++) {
    x += 100;
    x = x * 5 + i;
  }
  return x;
}

int main(void) {
  printf("mix4 %u\n", mix4(7));
  printf("skip4 %u %u\n", skip4(2), skip4(9));
  printf("sizes %zu\n", sizes());
  printf("pruned %u\n", pruned(5));
  printf("triangle %llu\n", (unsigned long long)triangle(1));
  printf("signed %lld\n", (long long)signed_range(9));
  printf("narrow %d\n", narrow(4, 7));
  uint64_t values[6] = {3, 1, 4, 1, 5, 9};
  printf("window %llu %llu\n", (unsigned long long)window(4, values),
         (unsigned long long)window(6, values));
  for (uint32_t x = 0; x < 10; x++) printf("first %u %u\n", x, first_match(x * 13));
  printf("labeled %u %u\n", labeled(9), labeled(40));
  printf("deferred %u\n", deferred(1));
  size_t same = 0;
  for (size_t step = 3; step < 3 + 2 * 32; step += 2) same += step * 11 * step + step * step;
  printf("dense %zu\n", same);
  return 0;
}

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define N 262144

static uint64_t src[N];
static uint64_t dst[N];

int main(void) {
    uint64_t x = 0x9e3779b97f4a7c15ULL;
    for (size_t i = 0; i < N; i++) {
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        src[i] = x;
    }
    size_t counts[256] = {0};
    for (size_t i = 0; i < N; i++) {
        counts[(src[i] >> 8) & 0xFF] += 1;
    }
    size_t start = 0;
    for (size_t b = 0; b < 256; b++) {
        size_t n = counts[b];
        counts[b] = start;
        start += n;
    }
    for (size_t i = 0; i < N; i++) {
        uint64_t v = src[i];
        size_t d = (v >> 8) & 0xFF;
        dst[counts[d]] = v;
        counts[d] += 1;
    }
    uint64_t acc = 0;
    for (size_t i = 0; i < N; i++) {
        acc = (acc ^ dst[i]) * 0x100000001b3ULL;
    }
    printf("checksum %lu\n", (unsigned long)acc);
    return 0;
}

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
void report(int32_t a, int32_t b, int32_t c, int32_t d, size_t e, int32_t f, int32_t g, int32_t h) {
    printf("%d %d %d %d %zu %d %d %d\n", a, b, c, d, e, f, g, h);
}
#ifdef STATIC_SIZES_ORACLE
int main(void) {
    enum { N = 3, K = 3, L = sizeof("abc") - 1 };
    int32_t V[N] = {1,2,3};
    const int32_t T[N] = {1,2,3};
    struct P { uint8_t d[N]; } p = {{4,5,6}};
    const uint8_t S[3] = {1,2,3};
    static uint8_t W[L];
    uint8_t X[K] = {7,8,9};
    enum { SIZE_N = 12, SHIFT = 3 };
    const uint8_t TABLE[5] = {1,2,3,4,5};
    int32_t B[64 / sizeof(int32_t)] = {0};
    int32_t D[SIZE_N / 4] = {0};
    uint8_t A[SIZE_N * 2 + 1] = {0};
    uint8_t SHIFTS[1 << SHIFT] = {0};
    uint8_t TAIL[sizeof(TABLE) - 1] = {0};
    uint8_t PICK[1 ? 3 : 5] = {0};
    uint8_t NESTED[2][2 * 2] = {{0}};
    uint8_t LANES[1 << SHIFT] = {0};
    uint8_t Generic[sizeof(uint32_t) * 4] = {0};
    B[15] = 16;
    D[2] = 3;
    A[24] = 25;
    SHIFTS[7] = 8;
    TAIL[3] = 4;
    PICK[2] = 3;
    NESTED[1][3] = 4;
    LANES[7] = 8;
    int32_t size_sum = B[15] + D[2] + A[24] + SHIFTS[7] + TAIL[3] + PICK[2] + NESTED[1][3] + LANES[7] + (int32_t)sizeof(Generic);
    report(V[2], T[1], p.d[2], S[1], sizeof(S), W[2], X[2], size_sum);
    return 0;
}
#endif

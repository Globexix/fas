#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
void report(int32_t a, int32_t b, int32_t c, int32_t d, size_t e, int32_t f, int32_t g) {
    printf("%d %d %d %d %zu %d %d\n", a, b, c, d, e, f, g);
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
    report(V[2], T[1], p.d[2], S[1], sizeof(S), W[2], X[2]);
    return 0;
}
#endif

#include <stdint.h>
#include <stdio.h>

extern int32_t cont_add(int32_t a, int32_t b);
extern int32_t cont_sum(int32_t a, int32_t b, int32_t c);
extern int32_t cont_arith(int32_t a, int32_t b);
extern int32_t cont_compare(int32_t a, int32_t b);
extern int32_t cont_if(int32_t a, int32_t b);
extern int32_t cont_while(int32_t n);
extern int32_t cont_for(int32_t n);
extern int32_t cont_call(int32_t a, int32_t b);
extern int32_t cont_init(int32_t a);
extern int32_t cont_blank(int32_t a, int32_t b);
extern int32_t cont_block_comment(int32_t a, int32_t b);

static int32_t oracle_while(int32_t n) {
    int32_t total = 0;
    for (int32_t i = 0; i < n; i += 1) {
        total = total + i;
    }
    return total;
}

static int32_t oracle_for(int32_t n) {
    int32_t total = 0;
    for (int32_t i = 0; i < n && total >= 0; i += 1) {
        total += i;
    }
    return total;
}

int main(void) {
    static const int32_t values[] = {-7, -2, 0, 1, 3, 5, 9, 100};
    static const int32_t counts[] = {0, 1, 5, 10};
    int failures = 0;
    for (unsigned ia = 0; ia < sizeof values / sizeof values[0]; ia++) {
        for (unsigned ib = 0; ib < sizeof values / sizeof values[0]; ib++) {
            int32_t a = values[ia];
            int32_t b = values[ib];
            int32_t c = values[(ia + ib) % 8];
            int32_t ok = 1;
            ok = ok && cont_add(a, b) == (a + b);
            ok = ok && cont_sum(a, b, c) == (a + b + c);
            ok = ok && cont_arith(a, b) == ((((a * b) / 2) % 5) & 15 | 32 ^ 3) << 1 >> 1;
            ok = ok && cont_compare(a, b) == (a < b && a != 0 || a >= b);
            ok = ok && cont_if(a, b) == (a > 0 && b > 0 || a == b ? 1 : 0);
            ok = ok && cont_call(a, b) == a + b + 1;
            ok = ok && cont_init(a) == a - 1;
            ok = ok && cont_blank(a, b) == a * b;
            ok = ok && cont_block_comment(a, b) == a + b;
            if (!ok) {
                printf("line continuation: mismatch at a=%d b=%d c=%d\n", a, b, c);
                failures++;
            }
        }
    }
    for (unsigned i = 0; i < sizeof counts / sizeof counts[0]; i++) {
        int32_t n = counts[i];
        if (cont_while(n) != oracle_while(n)) {
            printf("line continuation: while mismatch at n=%d\n", n);
            failures++;
        }
        if (cont_for(n) != oracle_for(n)) {
            printf("line continuation: for mismatch at n=%d\n", n);
            failures++;
        }
    }
    if (failures != 0) {
        return 1;
    }
    puts("line continuation: ok");
    return 0;
}

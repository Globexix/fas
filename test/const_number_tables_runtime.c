#include <stdio.h>

void dump_i64(long value) { printf("i64 %ld\n", value); }

void dump_u64(unsigned long value) { printf("u64 %lu\n", value); }

void dump_f64(double value) { printf("f64 %.17g\n", value); }

void dump_bytes(int label, const void *bytes, unsigned long count) {
    const unsigned char *p = bytes;
    printf("bytes %d %lu", label, count);
    for (unsigned long i = 0; i < count; i++) printf(" %02x", p[i]);
    printf("\n");
}

#ifndef NUMBER_ORACLE
extern int fas_run(void);
int main(void) {
    int result = fas_run();
    printf("result %d\n", result);
    return result;
}
#endif

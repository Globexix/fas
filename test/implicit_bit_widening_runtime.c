#include <stdio.h>

void dump_i64(long value) { printf("i64 %ld\n", value); }

void dump_u64(unsigned long value) { printf("u64 %lu\n", value); }

#ifndef BIT_WIDENING_ORACLE
extern int fas_run(void);
int main(void) {
    int result = fas_run();
    printf("result %d\n", result);
    return result;
}
#endif

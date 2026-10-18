#include <stdint.h>
#include <stdio.h>

static uint32_t c1(int16_t s, int16_t t) { return (uint32_t)(((int32_t)s / (int32_t)t) >> 1); }
static uint64_t c2(int8_t s, int8_t t) { return (uint64_t)((int64_t)s % (int64_t)t); }
static int32_t c3(int8_t s, int8_t t) { (void)t; return -(int32_t)s; }
static uint64_t c4(uint8_t s, uint8_t t) { (void)t; return ~(uint64_t)s; }
static int64_t c5(uint16_t s, uint16_t t) { return (int64_t)s - (int64_t)t; }
static int64_t c6(int16_t s, int16_t t) { return (int64_t)s * (int64_t)t; }
static uint32_t c7(uint8_t s, uint8_t t) { return ((uint32_t)s + (uint32_t)t) >> 1; }
static int64_t c8(int8_t s, int8_t t) { return ((int64_t)s + (int64_t)t) >> 1; }
static uint32_t c9(int16_t s, int16_t t) { (void)t; return (uint32_t)~(int32_t)s; }
static int32_t c10(int16_t s, int16_t t) { return (int32_t)s % (int32_t)t; }
static uint64_t c11(int16_t s, int16_t t) { return (uint64_t)-((int64_t)s + (int64_t)t); }
static int64_t c12(uint8_t s, uint8_t t) { return ((int64_t)s - (int64_t)t) / 3; }

int main(void) {
    printf("%u\n", c1(-32768, -1));
    printf("%u\n", c1(32767, -32768));
    printf("%u\n", c1(-300, 7));
    printf("%u\n", c1(12345, -3));
    printf("%lu\n", (unsigned long)c2(-128, -1));
    printf("%lu\n", (unsigned long)c2(-128, 3));
    printf("%lu\n", (unsigned long)c2(127, -5));
    printf("%lu\n", (unsigned long)c2(-7, 127));
    printf("%d\n", c3(-128, 1));
    printf("%d\n", c3(127, -1));
    printf("%d\n", c3(-1, -1));
    printf("%d\n", c3(0, 5));
    printf("%lu\n", (unsigned long)c4(0, 1));
    printf("%lu\n", (unsigned long)c4(255, 1));
    printf("%lu\n", (unsigned long)c4(128, 255));
    printf("%lu\n", (unsigned long)c4(7, 3));
    printf("%ld\n", (long)c5(0, 65535));
    printf("%ld\n", (long)c5(65535, 1));
    printf("%ld\n", (long)c5(40000, 65535));
    printf("%ld\n", (long)c5(1, 2));
    printf("%ld\n", (long)c6(-32768, -32768));
    printf("%ld\n", (long)c6(32767, 32767));
    printf("%ld\n", (long)c6(-1, -32768));
    printf("%ld\n", (long)c6(-300, 7));
    printf("%u\n", c7(255, 255));
    printf("%u\n", c7(255, 1));
    printf("%u\n", c7(0, 0));
    printf("%u\n", c7(128, 127));
    printf("%ld\n", (long)c8(-128, -128));
    printf("%ld\n", (long)c8(127, 127));
    printf("%ld\n", (long)c8(-1, -2));
    printf("%ld\n", (long)c8(-5, 3));
    printf("%u\n", c9(-32768, 0));
    printf("%u\n", c9(32767, 1));
    printf("%u\n", c9(-1, 1));
    printf("%u\n", c9(0, -7));
    printf("%d\n", c10(-32768, -1));
    printf("%d\n", c10(32767, -32768));
    printf("%d\n", c10(-7, 3));
    printf("%d\n", c10(12345, -7));
    printf("%lu\n", (unsigned long)c11(-32768, -32768));
    printf("%lu\n", (unsigned long)c11(32767, 32767));
    printf("%lu\n", (unsigned long)c11(-1, 1));
    printf("%lu\n", (unsigned long)c11(100, -200));
    printf("%ld\n", (long)c12(0, 3));
    printf("%ld\n", (long)c12(255, 255));
    printf("%ld\n", (long)c12(1, 255));
    printf("%ld\n", (long)c12(200, 7));
    return 0;
}

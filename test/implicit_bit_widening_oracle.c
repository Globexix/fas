#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

void dump_i64(long value);
void dump_u64(unsigned long value);

static const uint8_t U8S[] = {0, 1, 127, 255};
static const uint16_t U16S[] = {0, 1, 40000, 65535};
static const uint32_t U32S[] = {0, 3, 2147483648u, 4294967295u};
static const int16_t I16S[] = {0, -1, -32768, 32767};
static const int16_t I16T[] = {-7, 5, -32768, 32767};
static const uint32_t W32[] = {0, 77, 4294967295u};

#define COUNT(array) (sizeof(array) / sizeof((array)[0]))

static int32_t shr_u16(uint16_t a) { return (int32_t)(a >> 1); }
static int32_t and_u8(uint8_t b) { return (int32_t)(b & 0x7f); }
static uint32_t mask_u16(uint16_t a) { return (uint32_t)((a >> 4) & 0xf); }
static uint64_t div_u32(uint32_t a) { return (uint64_t)(a / 3u); }
static uint32_t rem_u8(uint8_t c) { return (uint32_t)(c % 10); }
static int32_t shr_i16(int16_t s) { return (int32_t)s >> 2; }
static int32_t and_i16(int16_t s, int16_t t) { return (int32_t)(s & t); }
static int32_t eq_masked(uint8_t b, uint32_t w) { return (uint32_t)(b & 0x7f) == w ? 1 : 0; }
static uint32_t or_div_u32(uint16_t a, uint32_t w) { return (uint32_t)(a / 3u) | w; }
static uint64_t or_div_u64(uint16_t a, uint32_t w) { return (uint64_t)((uint32_t)(a / 3u) | w); }
static uint64_t shr_div_u64(uint32_t a) { return (uint64_t)((uint32_t)(a / 3u) >> 1); }

static int oracle_run(void) {
    for (size_t i = 0; i < COUNT(U16S); i++) {
        dump_i64(shr_u16(U16S[i]));
        dump_i64((long)mask_u16(U16S[i]));
    }
    for (size_t i = 0; i < COUNT(U8S); i++) {
        dump_i64(and_u8(U8S[i]));
        dump_i64((long)rem_u8(U8S[i]));
    }
    for (size_t i = 0; i < COUNT(U32S); i++) {
        dump_u64(div_u32(U32S[i]));
        dump_u64(shr_div_u64(U32S[i]));
    }
    for (size_t i = 0; i < COUNT(I16S); i++) {
        dump_i64(shr_i16(I16S[i]));
    }
    for (size_t i = 0; i < COUNT(I16S); i++) {
        for (size_t j = 0; j < COUNT(I16T); j++) {
            dump_i64(and_i16(I16S[i], I16T[j]));
        }
    }
    for (size_t i = 0; i < COUNT(U8S); i++) {
        for (size_t j = 0; j < COUNT(W32); j++) {
            dump_i64(eq_masked(U8S[i], W32[j]));
        }
    }
    for (size_t i = 0; i < COUNT(U16S); i++) {
        for (size_t j = 0; j < COUNT(W32); j++) {
            dump_i64((long)or_div_u32(U16S[i], W32[j]));
            dump_u64(or_div_u64(U16S[i], W32[j]));
        }
    }
    return 0;
}

int main(void) {
    int result = oracle_run();
    printf("result %d\n", result);
    return result;
}

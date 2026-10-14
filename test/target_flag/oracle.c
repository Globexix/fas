#include <stdint.h>
#include <stdio.h>
#include <string.h>
int main(int argc, char **argv) {
    if (argc != 2) return 2;
    if (strcmp(argv[1], "main") == 0) {
        uint32_t buf[256];
        uint64_t sum = 0;
        for (uint32_t i = 0; i < 256; i++) buf[i] = i * 2654435761u + 12345u;
        for (uint32_t i = 0; i < 256; i++) sum += buf[i];
        printf("sum %lu\n", (unsigned long)sum);
        for (unsigned k = 0; k < 16; k++) printf("%u\n", 15u - k);
        return 0;
    }
    if (strcmp(argv[1], "intrinsics") == 0) {
        uint64_t bits = 0x9E3779B97F4A7C15ull;
        unsigned count = 0;
        for (unsigned i = 0; i < 64; i++) count += (unsigned)((bits >> i) & 1u);
        printf("%u %lu\n", count, (unsigned long)(0xABCDEF0123456789ull & ((1ull << 8) - 1)));
        return 0;
    }
    return 2;
}

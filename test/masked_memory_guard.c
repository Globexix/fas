#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

uint64_t masked_guard_u8x16_load(void *p);
uint64_t masked_guard_u8x16_tail_load(void *p, size_t n);
void masked_guard_u8x16_store(void *p);
uint64_t masked_guard_u32x8_load(void *p);
void masked_guard_u32x8_store(void *p);
uint64_t masked_guard_bool16_load(void *p);

static uint64_t hash_lanes(const uint64_t *lanes, size_t count)
{
    uint64_t h = 0;
    for (size_t i = 0; i < count; i++) h = h * UINT64_C(1315423911) + lanes[i];
    return h;
}

static int check_u8x16_load(uint8_t *edge)
{
    static const uint8_t data[5] = {11, 22, 33, 44, 55};
    uint8_t *base = edge - sizeof data;
    memcpy(base, data, sizeof data);
    uint64_t lanes[16];
    for (size_t i = 0; i < 16; i++) lanes[i] = i < 5 ? data[i] : 100 + i;
    if (masked_guard_u8x16_load(base) != hash_lanes(lanes, 16)) {
        fprintf(stderr, "masked memory guard: u8x16 masked load mismatch\n");
        return 1;
    }
    return 0;
}

static int check_u8x16_tail_load(uint8_t *edge)
{
    static const uint8_t data[5] = {11, 22, 33, 44, 55};
    uint8_t *base = edge - sizeof data;
    memcpy(base, data, sizeof data);
    uint64_t lanes[16];
    for (size_t i = 0; i < 16; i++) lanes[i] = i < 5 ? data[i] : 100 + i;
    if (masked_guard_u8x16_tail_load(base, 5) != hash_lanes(lanes, 16)) {
        fprintf(stderr, "masked memory guard: u8x16 runtime-mask masked load mismatch\n");
        return 1;
    }
    for (size_t i = 0; i < 16; i++) lanes[i] = 100 + i;
    if (masked_guard_u8x16_tail_load(edge, 0) != hash_lanes(lanes, 16)) {
        fprintf(stderr, "masked memory guard: u8x16 empty-mask masked load accessed memory\n");
        return 1;
    }
    return 0;
}

static int check_u8x16_store(uint8_t *edge)
{
    uint8_t *base = edge - 5;
    memset(base - 4, 0xA5, 4);
    memset(base, 0, 5);
    masked_guard_u8x16_store(base);
    static const uint8_t expected[5] = {1, 2, 3, 4, 5};
    for (int i = 1; i <= 4; i++) {
        if (base[-i] != 0xA5) {
            fprintf(stderr, "masked memory guard: u8x16 masked store wrote an inactive byte\n");
            return 1;
        }
    }
    if (memcmp(base, expected, sizeof expected) != 0) {
        fprintf(stderr, "masked memory guard: u8x16 masked store mismatch\n");
        return 1;
    }
    return 0;
}

static int check_u32x8_load(uint8_t *edge)
{
    static const uint32_t data[3] = {7001, 7002, 7003};
    uint8_t *base = edge - 12;
    memcpy(base, data, sizeof data);
    uint64_t lanes[8];
    for (size_t i = 0; i < 8; i++) lanes[i] = i < 3 ? data[i] : 1000 + i;
    if (masked_guard_u32x8_load(base) != hash_lanes(lanes, 8)) {
        fprintf(stderr, "masked memory guard: u32x8 masked load mismatch\n");
        return 1;
    }
    return 0;
}

static int check_u32x8_store(uint8_t *edge)
{
    uint8_t *base = edge - 12;
    memset(base, 0, 12);
    masked_guard_u32x8_store(base);
    static const uint32_t expected[3] = {501, 502, 503};
    uint32_t got[3];
    memcpy(got, base, sizeof got);
    if (memcmp(got, expected, sizeof expected) != 0) {
        fprintf(stderr, "masked memory guard: u32x8 masked store mismatch\n");
        return 1;
    }
    return 0;
}

static int check_bool16_load(uint8_t *edge)
{
    static const uint8_t fallback[16] = {1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1};
    uint8_t *base = edge - 3;
    base[0] = 0;
    base[1] = 1;
    base[2] = 0;
    uint64_t lanes[16];
    lanes[0] = 0;
    lanes[1] = 1;
    lanes[2] = 0;
    for (size_t i = 3; i < 16; i++) lanes[i] = fallback[i];
    if (masked_guard_bool16_load(base) != hash_lanes(lanes, 16)) {
        fprintf(stderr, "masked memory guard: bool x16 masked load mismatch\n");
        return 1;
    }
    return 0;
}

int main(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0) return 1;
    size_t page = (size_t)page_size;
    uint8_t *mapping = mmap(NULL, page * 2, PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED) return 1;
    if (mprotect(mapping + page, page, PROT_NONE) != 0) return 1;
    uint8_t *edge = mapping + page;
    int failed = 0;
    failed |= check_u8x16_load(edge);
    failed |= check_u8x16_tail_load(edge);
    failed |= check_u8x16_store(edge);
    failed |= check_u32x8_load(edge);
    failed |= check_u32x8_store(edge);
    failed |= check_bool16_load(edge);
    return failed;
}

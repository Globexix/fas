#define _GNU_SOURCE
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "simd_memory_cases.h"

extern uint64_t fas_null_masked_load(void);
extern void fas_null_masked_store(void);
extern uint64_t fas_null_gather(void);
extern void fas_null_scatter(void);
extern uint64_t fas_null_gather_bytes(void);
extern void fas_null_scatter_bytes(void);
extern uint64_t fas_bool_load(void *);
extern void fas_bool_store(void *);
extern uint64_t fas_guard_masked_load(void *);
extern uint64_t fas_guard_gather(void *, size_t);
extern uint64_t fas_edge_gather_i8(void *);
extern uint64_t fas_edge_gather_u64(void *);
extern uint64_t fas_edge_gather_bytes_i8(void *);
extern uint64_t fas_edge_gather_bytes_u64(void *);
extern void fas_edge_scatter_i8(void *);
extern void fas_edge_scatter_u64(void *);
extern void fas_edge_scatter_bytes_i8(void *);
extern void fas_edge_scatter_bytes_u64(void *);
extern void fas_overlap_scatter(void *);
extern void fas_overlap_scatter_bytes(void *);
extern void fas_alias_scatter(void *);
extern uint64_t fas_order_load(void);
extern void fas_order_store(void);

static uint32_t observer_events[16];
static size_t observer_count;
static uint32_t order_data[8];

void *simd_observe_addr(uint32_t tag)
{
    observer_events[observer_count++] = tag;
    return order_data;
}

uint32_t simd_observe_u32(uint32_t tag)
{
    observer_events[observer_count++] = tag;
    return tag;
}

bool simd_observe_bool(uint32_t tag)
{
    observer_events[observer_count++] = tag;
    return tag != 0u;
}

static uint64_t hash_lane(uint64_t hash, uint64_t value, bool first)
{
    return first ? value : hash * UINT64_C(1315423911) + value;
}

static uint64_t read_raw(const uint8_t *pointer, int size)
{
    uint64_t value = 0;
    for (int byte = 0; byte < size; ++byte)
        value |= (uint64_t)pointer[byte] << (byte * 8);
    return value;
}

static void write_raw(uint8_t *pointer, int size, uint64_t value)
{
    for (int byte = 0; byte < size; ++byte)
        pointer[byte] = (uint8_t)(value >> (byte * 8));
}

static int64_t lane_offset(const struct simd_case *test, int lane)
{
    switch (test->op) {
    case SIMD_MASKED_LOAD:
    case SIMD_MASKED_STORE:
        return (int64_t)lane * test->size;
    case SIMD_GATHER:
    case SIMD_SCATTER:
        return test->indices[lane] * test->size;
    case SIMD_GATHER_BYTES:
    case SIMD_SCATTER_BYTES:
        return test->indices[lane];
    }
    return 0;
}

static uint64_t expected_load(const struct simd_case *test, const uint8_t *base)
{
    uint64_t hash = 0;
    for (int lane = 0; lane < test->lanes; ++lane) {
        uint64_t value = (test->mask & (1u << lane))
            ? read_raw(base + lane_offset(test, lane), test->size)
            : test->fallback[lane];
        if (test->ty == SIMD_BOOL && (test->mask & (1u << lane)))
            value = value != 0;
        hash = hash_lane(hash, value, lane == 0);
    }
    return hash;
}

static int test_matrix(void)
{
    uint8_t memory[4096];
    uint8_t expected[sizeof(memory)];
    for (size_t index = 0; index < simd_case_count; ++index) {
        const struct simd_case *test = &simd_cases[index];
        for (size_t byte = 0; byte < sizeof(memory); ++byte)
            memory[byte] = (uint8_t)(byte * 37u + 11u);
        memcpy(expected, memory, sizeof(memory));
        uint8_t *base = memory + 512;
        if (test->load != NULL) {
            uint64_t got = test->load(base);
            uint64_t want = expected_load(test, base);
            if (got != want) {
                fprintf(stderr, "simd memory: %s got %llu expected %llu\n", test->name,
                        (unsigned long long)got, (unsigned long long)want);
                return 1;
            }
        } else {
            uint8_t *expected_base = expected + 512;
            for (int lane = 0; lane < test->lanes; ++lane)
                if (test->mask & (1u << lane))
                    write_raw(expected_base + lane_offset(test, lane), test->size,
                              test->values[lane]);
            test->store(base);
            if (memcmp(memory, expected, sizeof(memory)) != 0) {
                fprintf(stderr, "simd memory: %s changed unexpected bytes\n", test->name);
                return 1;
            }
        }
    }
    return 0;
}

static int test_null_masks(void)
{
    if (fas_null_masked_load() != UINT64_C(13) * UINT64_C(1315423911) * UINT64_C(1315423911) * UINT64_C(1315423911)
        + UINT64_C(17) * UINT64_C(1315423911) * UINT64_C(1315423911)
        + UINT64_C(19) * UINT64_C(1315423911) + UINT64_C(23)) return 1;
    if (fas_null_gather() != UINT64_C(43) * UINT64_C(1315423911) * UINT64_C(1315423911) * UINT64_C(1315423911)
        + UINT64_C(47) * UINT64_C(1315423911) * UINT64_C(1315423911)
        + UINT64_C(53) * UINT64_C(1315423911) + UINT64_C(59)) return 1;
    if (fas_null_gather_bytes() != UINT64_C(79) * UINT64_C(1315423911) * UINT64_C(1315423911) * UINT64_C(1315423911)
        + UINT64_C(83) * UINT64_C(1315423911) * UINT64_C(1315423911)
        + UINT64_C(89) * UINT64_C(1315423911) + UINT64_C(97)) return 1;
    fas_null_masked_store();
    fas_null_scatter();
    fas_null_scatter_bytes();
    return 0;
}

static int test_bool_bytes(void)
{
    uint8_t bytes[4] = {0, 2, 255, 0};
    uint64_t expected = UINT64_C(1315423911) * UINT64_C(1315423911) + UINT64_C(1315423911);
    if (fas_bool_load(bytes) != expected) {
        fprintf(stderr, "simd memory: bool load did not normalize nonzero bytes\n");
        return 1;
    }
    fas_bool_store(bytes);
    const uint8_t stored[4] = {1, 0, 1, 0};
    if (memcmp(bytes, stored, sizeof(stored)) != 0) {
        fprintf(stderr, "simd memory: bool store did not write 0/1 bytes\n");
        return 1;
    }
    return 0;
}

static int test_guard_pages(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0) return 1;
    uint8_t *mapping = mmap(NULL, (size_t)page_size * 2, PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED) return 1;
    if (mprotect(mapping + page_size, (size_t)page_size, PROT_NONE) != 0) return 1;
    uint8_t *protected_page = mapping + page_size;
    uint8_t *tail = protected_page - 12;
    write_raw(tail, 4, UINT32_C(101));
    write_raw(tail + 4, 4, UINT32_C(103));
    write_raw(tail + 8, 4, UINT32_C(107));
    uint64_t tail_hash = UINT64_C(101) * UINT64_C(1315423911) * UINT64_C(1315423911)
                         + UINT64_C(103) * UINT64_C(1315423911) + UINT64_C(107);
    tail_hash = tail_hash * UINT64_C(1315423911) + UINT64_C(305419896);
    if (fas_guard_masked_load(tail) != tail_hash) {
        fprintf(stderr, "simd memory: masked load touched its inactive guard lane\n");
        return 1;
    }
    write_raw(mapping, 4, UINT32_C(109));
    write_raw(mapping + 4, 4, UINT32_C(113));
    write_raw(mapping + 8, 4, UINT32_C(127));
    uint64_t gather_hash = UINT64_C(109) * UINT64_C(1315423911) * UINT64_C(1315423911)
                           + UINT64_C(33) * UINT64_C(1315423911) + UINT64_C(113);
    gather_hash = gather_hash * UINT64_C(1315423911) + UINT64_C(127);
    if (fas_guard_gather(mapping, (size_t)page_size) != gather_hash) {
        fprintf(stderr, "simd memory: gather touched its inactive guard lane\n");
        return 1;
    }
    munmap(mapping, (size_t)page_size * 2);
    return 0;
}

static void fill_edge(uint8_t *memory, size_t length)
{
    for (size_t index = 0; index < length; ++index)
        memory[index] = (uint8_t)(index * 29u + 7u);
}

static int check_hash(const char *name, uint64_t got, const int64_t offsets[4],
                      uint8_t *base, bool scale)
{
    uint64_t expected = 0;
    for (int lane = 0; lane < 4; ++lane) {
        uint64_t value = read_raw(base + (scale ? offsets[lane] * 4 : offsets[lane]), 4);
        expected = hash_lane(expected, value, lane == 0);
    }
    if (got == expected) return 0;
    fprintf(stderr, "simd memory: %s address normalization mismatch\n", name);
    return 1;
}

static int check_scatter(void (*store)(void *), const char *name,
                         const int64_t offsets[4], bool scale, size_t base_offset)
{
    static const uint32_t values[4] = {
        UINT32_C(287454020), UINT32_C(1432778632), UINT32_C(2578103244), UINT32_C(3723427584)
    };
    uint8_t memory[128];
    uint8_t expected[sizeof(memory)];
    fill_edge(memory, sizeof(memory));
    memcpy(expected, memory, sizeof(memory));
    uint8_t *base = memory + base_offset;
    for (int lane = 0; lane < 4; ++lane)
        write_raw(expected + base_offset + (scale ? offsets[lane] * 4 : offsets[lane]), 4,
                  values[lane]);
    store(base);
    if (memcmp(memory, expected, sizeof(memory)) == 0) return 0;
    fprintf(stderr, "simd memory: %s scatter normalization mismatch\n", name);
    return 1;
}

static int test_address_edges(void)
{
    uint8_t memory[128];
    const int64_t signed_indices[4] = {-1, 0, 1, 2};
    const int64_t wide_indices[4] = {0, 0, 1, 2};
    const int64_t byte_offsets[4] = {-1, 0, 1, 2};
    fill_edge(memory, sizeof(memory));
    uint8_t *scaled_base = memory + 32;
    uint8_t *byte_base = memory + 33;
    if (check_hash("gather i8", fas_edge_gather_i8(scaled_base), signed_indices, scaled_base, true)) return 1;
    if (check_hash("gather u64 wrap", fas_edge_gather_u64(scaled_base), wide_indices, scaled_base, true)) return 1;
    if (check_hash("gather_bytes i8", fas_edge_gather_bytes_i8(byte_base), byte_offsets, byte_base, false)) return 1;
    if (check_hash("gather_bytes u64 wrap", fas_edge_gather_bytes_u64(byte_base), byte_offsets, byte_base, false)) return 1;
    if (check_scatter(fas_edge_scatter_i8, "scatter i8", signed_indices, true, 32)) return 1;
    if (check_scatter(fas_edge_scatter_u64, "scatter u64 wrap", wide_indices, true, 32)) return 1;
    if (check_scatter(fas_edge_scatter_bytes_i8, "scatter_bytes i8", byte_offsets, false, 33)) return 1;
    if (check_scatter(fas_edge_scatter_bytes_u64, "scatter_bytes u64 wrap", byte_offsets, false, 33)) return 1;
    return 0;
}

static int test_overlap_and_alias(void)
{
    uint8_t bytes[24];
    uint8_t expected[sizeof(bytes)];
    static const uint32_t values[4] = {
        UINT32_C(287454020), UINT32_C(1432778632), UINT32_C(2578103244), UINT32_C(3723427584)
    };
    fill_edge(bytes, sizeof(bytes));
    memcpy(expected, bytes, sizeof(bytes));
    static const int64_t byte_offsets[4] = {0, 2, 2, 4};
    for (int lane = 0; lane < 4; ++lane)
        write_raw(expected + byte_offsets[lane], 4, values[lane]);
    fas_overlap_scatter_bytes(bytes);
    if (memcmp(bytes, expected, sizeof(bytes)) != 0) {
        fprintf(stderr, "simd memory: overlapping byte scatter did not preserve lane order\n");
        return 1;
    }
    fill_edge(bytes, sizeof(bytes));
    memcpy(expected, bytes, sizeof(bytes));
    static const int64_t scatter_indices[4] = {0, 1, 1, 2};
    for (int lane = 0; lane < 4; ++lane)
        write_raw(expected + scatter_indices[lane] * 4, 4, values[lane]);
    fas_overlap_scatter(bytes);
    if (memcmp(bytes, expected, sizeof(bytes)) != 0) {
        fprintf(stderr, "simd memory: overlapping scatter did not preserve lane order\n");
        return 1;
    }
    uint32_t alias[4] = {UINT32_C(0x10203040), UINT32_C(0x50607080), UINT32_C(0x90a0b0c0), UINT32_C(0xd0e0f001)};
    uint32_t snapshot[4];
    memcpy(snapshot, alias, sizeof(alias));
    for (int lane = 0; lane < 4; ++lane)
        memcpy(&alias[lane == 0 ? 1 : lane == 1 ? 0 : lane], &snapshot[lane], sizeof(uint32_t));
    uint32_t actual[4] = {UINT32_C(0x10203040), UINT32_C(0x50607080), UINT32_C(0x90a0b0c0), UINT32_C(0xd0e0f001)};
    fas_alias_scatter(actual);
    if (memcmp(actual, alias, sizeof(alias)) != 0) {
        fprintf(stderr, "simd memory: scatter did not capture aliased values before stores\n");
        return 1;
    }
    return 0;
}

static int test_evaluation_order(void)
{
    const uint32_t load_order[3] = {1, 2, 3};
    observer_count = 0;
    fas_order_load();
    if (observer_count != 3 || memcmp(observer_events, load_order, sizeof(load_order)) != 0) {
        fprintf(stderr, "simd memory: load argument evaluation order/count mismatch\n");
        return 1;
    }
    const uint32_t store_order[4] = {1, 2, 3, 4};
    observer_count = 0;
    fas_order_store();
    if (observer_count != 4 || memcmp(observer_events, store_order, sizeof(store_order)) != 0) {
        fprintf(stderr, "simd memory: store argument evaluation order/count mismatch\n");
        return 1;
    }
    return 0;
}

int main(void)
{
    int result = test_matrix() || test_null_masks() || test_bool_bytes()
                 || test_guard_pages() || test_address_edges()
                 || test_overlap_and_alias() || test_evaluation_order();
    if (result) return result;
    puts("simd memory: C oracle and semantic battery passed");
    return 0;
}

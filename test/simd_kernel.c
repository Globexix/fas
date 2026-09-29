#define _GNU_SOURCE
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <x86intrin.h>

extern uint64_t sum_xor_u32(void *, size_t);
extern void lookup_u32(void *, void *, void *, size_t);

struct guarded_region {
    uint8_t *mapping;
    size_t mapping_size;
    uint8_t *data;
};

static uint64_t measure_sink;

static uint32_t random_next(uint32_t *state)
{
    uint32_t value = *state;
    value ^= value << 13;
    value ^= value >> 17;
    value ^= value << 5;
    *state = value;
    return value;
}

static struct guarded_region guarded_region_create(size_t length, size_t trailing)
{
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0) abort();
    size_t page = (size_t)page_size;
    size_t needed = length + trailing;
    size_t accessible_pages = (needed + page - 1) / page;
    if (accessible_pages == 0) accessible_pages = 1;
    size_t accessible_size = accessible_pages * page;
    size_t mapping_size = accessible_size + page;
    uint8_t *mapping = mmap(NULL, mapping_size, PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED) abort();
    if (mprotect(mapping + accessible_size, page, PROT_NONE) != 0) abort();
    return (struct guarded_region){
        .mapping = mapping,
        .mapping_size = mapping_size,
        .data = mapping + accessible_size - length - trailing,
    };
}

static void guarded_region_destroy(struct guarded_region region)
{
    if (munmap(region.mapping, region.mapping_size) != 0) abort();
}

static uint32_t read_u32(const uint8_t *bytes)
{
    uint32_t value;
    memcpy(&value, bytes, sizeof(value));
    return value;
}

static void write_u32(uint8_t *bytes, uint32_t value)
{
    memcpy(bytes, &value, sizeof(value));
}

__attribute__((noinline)) static uint64_t c_sum_xor_u32(const void *data, size_t n)
{
    const uint8_t *bytes = data;
    uint32_t sum = 0;
    uint32_t bits = 0;
    for (size_t i = 0; i < n; ++i) {
        uint32_t value = read_u32(bytes + i * sizeof(value));
        sum += value;
        bits ^= value;
    }
    return ((uint64_t)sum << 32) | bits;
}

__attribute__((noinline)) static void c_lookup_u32(const void *table, const void *indices,
                                                    void *output, size_t n)
{
    const uint8_t *table_bytes = table;
    const uint8_t *index_bytes = indices;
    uint8_t *output_bytes = output;
    for (size_t i = 0; i < n; ++i) {
        uint8_t index = index_bytes[i];
        write_u32(output_bytes + i * sizeof(uint32_t),
                  read_u32(table_bytes + (size_t)index * sizeof(uint32_t)));
    }
}

static void initialize_inputs(uint8_t *input, uint8_t *indices, size_t n,
                              uint32_t *random_state)
{
    for (size_t i = 0; i < n; ++i) {
        write_u32(input + i * sizeof(uint32_t), random_next(random_state));
        indices[i] = (uint8_t)random_next(random_state);
    }
}

static int run_case(size_t n, uint8_t *table, uint32_t *random_state,
                    bool random_trailing)
{
    size_t input_trailing = random_trailing ? random_next(random_state) & 15u : 0;
    size_t indices_trailing = random_trailing ? random_next(random_state) & 15u : 0;
    size_t output_trailing = random_trailing ? random_next(random_state) & 15u : 0;
    struct guarded_region input = guarded_region_create(n * sizeof(uint32_t), input_trailing);
    struct guarded_region indices = guarded_region_create(n, indices_trailing);
    struct guarded_region output = guarded_region_create(n * sizeof(uint32_t), output_trailing);
    uint8_t *expected = malloc(n == 0 ? 1 : n * sizeof(uint32_t));
    if (expected == NULL) abort();
    initialize_inputs(input.data, indices.data, n, random_state);

    uint64_t fas_sum = sum_xor_u32(input.data, n);
    uint64_t c_sum = c_sum_xor_u32(input.data, n);
    if (fas_sum != c_sum) {
        fprintf(stderr, "simd kernel: sum_xor mismatch at n=%zu: got %" PRIu64
                        " expected %" PRIu64 "\n", n, fas_sum, c_sum);
        free(expected);
        guarded_region_destroy(output);
        guarded_region_destroy(indices);
        guarded_region_destroy(input);
        return 1;
    }

    lookup_u32(table, indices.data, output.data, n);
    c_lookup_u32(table, indices.data, expected, n);
    if (memcmp(output.data, expected, n * sizeof(uint32_t)) != 0) {
        fprintf(stderr, "simd kernel: lookup mismatch at n=%zu\n", n);
        free(expected);
        guarded_region_destroy(output);
        guarded_region_destroy(indices);
        guarded_region_destroy(input);
        return 1;
    }

    free(expected);
    guarded_region_destroy(output);
    guarded_region_destroy(indices);
    guarded_region_destroy(input);
    return 0;
}

static uint64_t read_cycles(void)
{
    unsigned auxiliary;
    _mm_lfence();
    uint64_t cycles = __rdtscp(&auxiliary);
    _mm_lfence();
    return cycles;
}

static void report_measurement(const char *name, double fas_cycles, double c_cycles)
{
    printf("simd kernel O2 %s: Fas %.2f cycles/element, C %.2f cycles/element\n",
           name, fas_cycles, c_cycles);
}

static void measure(uint8_t *table)
{
    const size_t n = 65536;
    const unsigned rounds = 96;
    struct guarded_region input = guarded_region_create(n * sizeof(uint32_t), 7);
    struct guarded_region indices = guarded_region_create(n, 5);
    struct guarded_region output = guarded_region_create(n * sizeof(uint32_t), 3);
    uint32_t state = UINT32_C(0x75310a9d);
    initialize_inputs(input.data, indices.data, n, &state);

    uint64_t start = read_cycles();
    for (unsigned i = 0; i < rounds; ++i)
        measure_sink ^= sum_xor_u32(input.data, n);
    uint64_t end = read_cycles();
    double fas_sum_cycles = (double)(end - start) / ((double)rounds * (double)n);

    start = read_cycles();
    for (unsigned i = 0; i < rounds; ++i)
        measure_sink ^= c_sum_xor_u32(input.data, n);
    end = read_cycles();
    double c_sum_cycles = (double)(end - start) / ((double)rounds * (double)n);
    report_measurement("sum_xor_u32", fas_sum_cycles, c_sum_cycles);

    start = read_cycles();
    for (unsigned i = 0; i < rounds; ++i) {
        lookup_u32(table, indices.data, output.data, n);
        measure_sink ^= read_u32(output.data + (i % n) * sizeof(uint32_t));
    }
    end = read_cycles();
    double fas_lookup_cycles = (double)(end - start) / ((double)rounds * (double)n);

    start = read_cycles();
    for (unsigned i = 0; i < rounds; ++i) {
        c_lookup_u32(table, indices.data, output.data, n);
        measure_sink ^= read_u32(output.data + (i % n) * sizeof(uint32_t));
    }
    end = read_cycles();
    double c_lookup_cycles = (double)(end - start) / ((double)rounds * (double)n);
    report_measurement("lookup_u32", fas_lookup_cycles, c_lookup_cycles);

    guarded_region_destroy(output);
    guarded_region_destroy(indices);
    guarded_region_destroy(input);
}

int main(int argc, char **argv)
{
    uint32_t state = UINT32_C(0x5f3759df);
    struct guarded_region table = guarded_region_create(256 * sizeof(uint32_t), 0);
    for (size_t i = 0; i < 256; ++i)
        write_u32(table.data + i * sizeof(uint32_t), random_next(&state));

    const size_t strict_lengths[] = {0, 1, 7, 8, 9, 15, 16, 17, 4099};
    for (size_t i = 0; i < sizeof(strict_lengths) / sizeof(strict_lengths[0]); ++i)
        if (run_case(strict_lengths[i], table.data, &state, false)) return 1;
    for (size_t i = 0; i < 1000; ++i) {
        size_t n = random_next(&state) % 4100u;
        if (run_case(n, table.data, &state, true)) return 1;
    }
    puts("simd kernel: 1000 fixed-seed lengths and guard-page tails passed");

    if (argc == 2 && strcmp(argv[1], "--measure") == 0)
        measure(table.data);
    guarded_region_destroy(table);
    return 0;
}

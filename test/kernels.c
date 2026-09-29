#define _GNU_SOURCE
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <x86intrin.h>

extern size_t fas_filter_scalar(void *, void *, size_t, uint32_t);
extern size_t fas_filter_simd(void *, void *, size_t, uint32_t);
extern void fas_records_transform(void *, size_t);
extern void fas_scale_pixels(void *, void *, size_t, uint8_t);
extern uint64_t fas_mask_reduce(void *, size_t, uint16_t);

static uint32_t random_state = UINT32_C(0x6d2b79f5);
static volatile uint64_t bench_sink;
static volatile uint16_t benchmark_target;

static uint32_t random_next(void)
{
    uint32_t value = random_state;
    value ^= value << 13;
    value ^= value >> 17;
    value ^= value << 5;
    random_state = value;
    return value;
}

static uint32_t transform(uint32_t value)
{
    return (value ^ UINT32_C(0xa5a55a5a)) * UINT32_C(3) + UINT32_C(7);
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

__attribute__((noinline)) static size_t c_filter_scalar(const uint32_t *input,
                                                         uint32_t *output,
                                                         size_t n,
                                                         uint32_t threshold)
{
    size_t written = 0;
    for (size_t i = 0; i < n; ++i)
        if (input[i] > threshold) output[written++] = input[i];
    return written;
}

__attribute__((noinline)) static void c_records_transform(uint8_t *records, size_t n)
{
    for (size_t i = 0; i < n; ++i) {
        uint8_t *field = records + i * 12 + 8;
        write_u32(field, transform(read_u32(field)));
    }
}

__attribute__((noinline)) static void c_scale_pixels(const uint8_t *input,
                                                       uint8_t *output,
                                                       size_t n,
                                                       uint8_t factor)
{
    for (size_t i = 0; i < n; ++i) {
        uint16_t scaled = (uint16_t)(((uint16_t)input[i] * factor) >> 8);
        uint16_t raised = scaled + 37u;
        if (raised > UINT8_MAX) raised = UINT8_MAX;
        output[i] = raised > 19u ? (uint8_t)(raised - 19u) : 0;
    }
}

__attribute__((noinline)) static uint64_t c_mask_reduce(const uint16_t *input,
                                                         size_t n,
                                                         uint16_t target)
{
    uint64_t count = 0;
    uint16_t bits = 0;
    for (size_t i = 0; i < n; ++i) {
        if (input[i] == target) {
            ++count;
            bits ^= input[i];
        }
    }
    return (count << 32) | bits;
}

static int check_filter(size_t n, const uint32_t *input)
{
    const uint32_t threshold = UINT32_C(0x80000000);
    size_t expected_count = 0;
    uint32_t *expected = calloc(n == 0 ? 1 : n, sizeof(*expected));
    uint32_t *scalar = malloc((n == 0 ? 1 : n) * sizeof(*scalar));
    uint32_t *simd = malloc((n == 0 ? 1 : n) * sizeof(*simd));
    if (expected == NULL || scalar == NULL || simd == NULL) abort();
    for (size_t i = 0; i < n; ++i)
        if (input[i] > threshold) expected[expected_count++] = input[i];
    memset(scalar, 0xa5, (n == 0 ? 1 : n) * sizeof(*scalar));
    memset(simd, 0xa5, (n == 0 ? 1 : n) * sizeof(*simd));
    size_t scalar_count = fas_filter_scalar((void *)input, scalar, n, threshold);
    size_t simd_count = fas_filter_simd((void *)input, simd, n, threshold);
    if (scalar_count != expected_count || simd_count != expected_count ||
        memcmp(scalar, expected, expected_count * sizeof(*expected)) != 0 ||
        memcmp(simd, expected, expected_count * sizeof(*expected)) != 0) {
        fprintf(stderr, "kernels: compaction mismatch at n=%zu\n", n);
        free(simd);
        free(scalar);
        free(expected);
        return 1;
    }
    for (size_t i = expected_count * sizeof(*expected); i < n * sizeof(*expected); ++i) {
        if (((const uint8_t *)scalar)[i] != 0xa5 || ((const uint8_t *)simd)[i] != 0xa5) {
            fprintf(stderr, "kernels: compaction wrote past its prefix at n=%zu\n", n);
            free(simd);
            free(scalar);
            free(expected);
            return 1;
        }
    }
    free(simd);
    free(scalar);
    free(expected);
    return 0;
}

static int check_records(size_t n)
{
    const size_t guard = 16;
    const size_t size = n * 12;
    uint8_t *records = malloc(size + guard * 2);
    uint8_t *expected = malloc(size + guard * 2);
    if (records == NULL || expected == NULL) abort();
    for (size_t i = 0; i < size + guard * 2; ++i) records[i] = (uint8_t)random_next();
    memcpy(expected, records, size + guard * 2);
    for (size_t i = 0; i < n; ++i) {
        uint8_t *field = expected + guard + i * 12 + 8;
        write_u32(field, transform(read_u32(field)));
    }
    fas_records_transform(records + guard, n);
    if (memcmp(records, expected, size + guard * 2) != 0) {
        fprintf(stderr, "kernels: packed record mismatch at n=%zu\n", n);
        free(expected);
        free(records);
        return 1;
    }
    free(expected);
    free(records);
    return 0;
}

static int check_pixels(size_t n)
{
    const size_t guard = 16;
    uint8_t *input = malloc(n + guard * 2);
    uint8_t *output = malloc(n + guard * 2);
    uint8_t *expected = malloc(n + guard * 2);
    uint8_t factor = (uint8_t)random_next();
    if (input == NULL || output == NULL || expected == NULL) abort();
    for (size_t i = 0; i < n + guard * 2; ++i) {
        input[i] = (uint8_t)random_next();
        output[i] = 0xa5;
        expected[i] = 0xa5;
    }
    for (size_t i = 0; i < n; ++i) {
        uint16_t scaled = (uint16_t)(((uint16_t)input[guard + i] * factor) >> 8);
        uint16_t raised = scaled + 37u;
        if (raised > UINT8_MAX) raised = UINT8_MAX;
        expected[guard + i] = raised > 19u ? (uint8_t)(raised - 19u) : 0;
    }
    fas_scale_pixels(input + guard, output + guard, n, factor);
    if (memcmp(output, expected, n + guard * 2) != 0) {
        fprintf(stderr, "kernels: saturation/scaling mismatch at n=%zu\n", n);
        free(expected);
        free(output);
        free(input);
        return 1;
    }
    free(expected);
    free(output);
    free(input);
    return 0;
}

static int check_mask_reduce(size_t n)
{
    const uint16_t target = UINT16_C(0x1234);
    uint16_t *input = malloc((n == 0 ? 1 : n) * sizeof(*input));
    uint64_t expected_count = 0;
    uint16_t expected_xor = 0;
    if (input == NULL) abort();
    for (size_t i = 0; i < n; ++i) {
        input[i] = (uint16_t)random_next();
        if (i % 11 == 0 || i % 37 == 0) input[i] = target;
        if (input[i] == target) {
            expected_count++;
            expected_xor ^= input[i];
        }
    }
    uint64_t got = fas_mask_reduce(input, n, target);
    uint64_t expected = (expected_count << 32) | expected_xor;
    if (got != expected) {
        fprintf(stderr, "kernels: mask/reduction mismatch at n=%zu: got=%" PRIu64
                        " expected=%" PRIu64 "\n", n, got, expected);
        free(input);
        return 1;
    }
    free(input);
    return 0;
}

static int run_case(size_t n)
{
    uint32_t *input = malloc((n == 0 ? 1 : n) * sizeof(*input));
    if (input == NULL) abort();
    for (size_t i = 0; i < n; ++i) input[i] = random_next();
    int failed = check_filter(n, input) || check_records(n) ||
                 check_pixels(n) || check_mask_reduce(n);
    free(input);
    return failed;
}

static uint64_t read_cycles(void)
{
    unsigned auxiliary;
    _mm_lfence();
    uint64_t cycles = __rdtscp(&auxiliary);
    _mm_lfence();
    return cycles;
}

static int compare_u64(const void *left, const void *right)
{
    uint64_t a = *(const uint64_t *)left;
    uint64_t b = *(const uint64_t *)right;
    return (a > b) - (a < b);
}

struct benchmark_case {
    unsigned kind;
    unsigned implementation;
    size_t n;
    const void *input;
    void *output;
};

static void run_benchmark_case(void *opaque)
{
    struct benchmark_case *test = opaque;
    if (test->kind == 0) {
        size_t count;
        if (test->implementation == 0)
            count = c_filter_scalar(test->input, test->output, test->n, UINT32_C(0x80000000));
        else if (test->implementation == 1)
            count = fas_filter_scalar((void *)test->input, test->output, test->n, UINT32_C(0x80000000));
        else
            count = fas_filter_simd((void *)test->input, test->output, test->n, UINT32_C(0x80000000));
        bench_sink ^= (uint64_t)count + ((uint32_t *)test->output)[count / 2];
    } else if (test->kind == 1) {
        uint8_t *records = test->output;
        if (test->implementation == 0) c_records_transform(records, test->n);
        else fas_records_transform(records, test->n);
        bench_sink ^= records[(bench_sink + 8) % (test->n * 12)];
    } else if (test->kind == 2) {
        if (test->implementation == 0)
            c_scale_pixels(test->input, test->output, test->n, UINT8_C(197));
        else
            fas_scale_pixels((void *)test->input, test->output, test->n, UINT8_C(197));
        bench_sink ^= ((uint8_t *)test->output)[bench_sink % test->n];
    } else {
        uint16_t target = benchmark_target;
        uint64_t result = test->implementation == 0
                              ? c_mask_reduce(test->input, test->n, target)
                              : fas_mask_reduce((void *)test->input, test->n, target);
        bench_sink ^= result;
    }
}

static double measure_case(unsigned kind,
                           unsigned implementation,
                           size_t n,
                           const void *input,
                           void *output)
{
    const unsigned repetitions = 64;
    uint64_t samples[7];
    struct benchmark_case test = {kind, implementation, n, input, output};
    for (size_t sample = 0; sample < 7; ++sample) {
        uint64_t start = read_cycles();
        for (unsigned repetition = 0; repetition < repetitions; ++repetition) {
            if (kind == 3)
                benchmark_target = (repetition & 1u) ? UINT16_C(0x1235) : UINT16_C(0x1234);
            run_benchmark_case(&test);
        }
        samples[sample] = read_cycles() - start;
    }
    qsort(samples, 7, sizeof(samples[0]), compare_u64);
    return (double)samples[3] / ((double)repetitions * (double)n);
}

static int measure(void)
{
    const size_t n = 32768;
    uint32_t *filter_input = malloc(n * sizeof(*filter_input));
    uint32_t *filter_output = malloc(n * sizeof(*filter_output));
    uint8_t *records = malloc(n * 12);
    uint8_t *pixels = malloc(n);
    uint8_t *pixel_output = malloc(n);
    uint16_t *mask_input = malloc(n * sizeof(*mask_input));
    if (filter_input == NULL || filter_output == NULL || records == NULL ||
        pixels == NULL || pixel_output == NULL || mask_input == NULL) abort();
    for (size_t i = 0; i < n; ++i) {
        filter_input[i] = random_next();
        pixels[i] = (uint8_t)random_next();
        mask_input[i] = i % 11 == 0 ? UINT16_C(0x1234)
                                    : i % 13 == 0 ? UINT16_C(0x1235) : (uint16_t)random_next();
        for (size_t byte = 0; byte < 12; ++byte) records[i * 12 + byte] = (uint8_t)random_next();
    }
    double filter_c = measure_case(0, 0, n, filter_input, filter_output);
    double filter_scalar = measure_case(0, 1, n, filter_input, filter_output);
    double filter_simd = measure_case(0, 2, n, filter_input, filter_output);
    double records_c = measure_case(1, 0, n, records, records);
    double records_fas = measure_case(1, 1, n, records, records);
    double pixels_c = measure_case(2, 0, n, pixels, pixel_output);
    double pixels_fas = measure_case(2, 1, n, pixels, pixel_output);
    double reduce_c = measure_case(3, 0, n, mask_input, NULL);
    double reduce_fas = measure_case(3, 1, n, mask_input, NULL);
    printf("kernels O2 compaction cycles/element: C %.2f, Fas scalar %.2f, Fas SIMD %.2f\n",
           filter_c, filter_scalar, filter_simd);
    printf("kernels O2 records cycles/record: C %.2f, Fas %.2f\n", records_c, records_fas);
    printf("kernels O2 saturation cycles/pixel: C %.2f, Fas %.2f\n", pixels_c, pixels_fas);
    printf("kernels O2 mask/reduction cycles/element: C %.2f, Fas %.2f\n", reduce_c, reduce_fas);
    free(mask_input);
    free(pixel_output);
    free(pixels);
    free(records);
    free(filter_output);
    free(filter_input);
    return 0;
}

int main(int argc, char **argv)
{
    const size_t lengths[] = {0, 1, 3, 4, 5, 7, 8, 9, 15, 16, 17, 4099};
    for (size_t i = 0; i < sizeof(lengths) / sizeof(lengths[0]); ++i)
        if (run_case(lengths[i])) return 1;
    for (size_t i = 0; i < 128; ++i)
        if (run_case(random_next() % 4100u)) return 1;
    puts("kernels: 12 boundary lengths and 128 fixed-seed random lengths passed");
    if (argc == 2 && strcmp(argv[1], "--measure") == 0) return measure();
    return 0;
}

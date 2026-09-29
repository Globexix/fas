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

extern uint32_t lowering_load32_le(void *);
extern bool lowering_short_circuit(void *, bool);
extern uint64_t lowering_masked_load(void *, bool);
extern uint64_t lowering_masked_none(void *);
extern uint64_t lowering_div_invariant(void *, size_t, uint32_t);
extern void lowering_add_recurrence(void *, void *, size_t);
extern void lowering_copy_stack(void *, void *, size_t);

static volatile uintptr_t stack_low = UINTPTR_MAX;
static volatile uintptr_t stack_high;

__attribute__((noinline)) void lowering_stack_sample(void)
{
    uintptr_t stack_pointer;
    __asm__ volatile("mov %%rsp, %0" : "=r"(stack_pointer));
    if (stack_pointer < stack_low) stack_low = stack_pointer;
    if (stack_pointer > stack_high) stack_high = stack_pointer;
}

static uint64_t packed4(uint32_t a, uint32_t b, uint32_t c, uint32_t d)
{
    return (uint64_t)(uint16_t)a |
           (uint64_t)(uint16_t)b << 16 |
           (uint64_t)(uint16_t)c << 32 |
           (uint64_t)(uint16_t)d << 48;
}

static int run_probes(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0) return 1;
    size_t page = (size_t)page_size;
    uint8_t *mapping = mmap(NULL, page * 2, PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED) return 1;
    if (mprotect(mapping + page, page, PROT_NONE) != 0) return 1;
    uint8_t *edge = mapping + page - 4;
    edge[0] = 0x78;
    edge[1] = 0x56;
    edge[2] = 0x34;
    edge[3] = 0x12;
    if (lowering_load32_le(edge) != UINT32_C(0x12345678)) {
        fprintf(stderr, "lowering probes: byte widening mismatch\n");
        return 1;
    }
    void *inaccessible = mapping + page;
    if (!lowering_short_circuit(inaccessible, 0)) {
        fprintf(stderr, "lowering probes: short-circuit load result mismatch\n");
        return 1;
    }
    if (lowering_masked_none(inaccessible) != packed4(13, 17, 19, 23)) {
        fprintf(stderr, "lowering probes: all-false masked load mismatch\n");
        return 1;
    }
    uint32_t *last_three = (uint32_t *)(mapping + page - 12);
    last_three[0] = 101;
    last_three[1] = 103;
    last_three[2] = 107;
    if (lowering_masked_load(last_three, 0) != packed4(101, 103, 107, 23)) {
        fprintf(stderr, "lowering probes: inactive masked lane accessed a protected page\n");
        return 1;
    }
    if (lowering_div_invariant(NULL, 0, 0) != 0) {
        fprintf(stderr, "lowering probes: zero-trip division touched the divisor\n");
        return 1;
    }
    if (munmap(mapping, page * 2) != 0) return 1;

    uint32_t input[257];
    uint32_t output[257];
    for (size_t i = 0; i < 257; ++i) {
        input[i] = (uint32_t)(i * 17u + 3u);
        output[i] = 0;
    }
    lowering_add_recurrence(input, output, 257);
    for (size_t i = 0; i < 257; ++i)
        if (output[i] != input[i] + 1u) {
            fprintf(stderr, "lowering probes: loop recurrence mismatch at %zu\n", i);
            return 1;
        }

    uint64_t source[512];
    uint64_t destination[512];
    for (size_t i = 0; i < 512; ++i) {
        source[i] = UINT64_C(0xfedcba9876543210) ^ (uint64_t)i;
        destination[i] = 0;
    }
    stack_low = UINTPTR_MAX;
    stack_high = 0;
    const size_t repetitions = 1000000;
    lowering_copy_stack(destination, source, repetitions);
    if (memcmp(destination, source, sizeof(source)) != 0) {
        fprintf(stderr, "lowering probes: 4 KiB copy mismatch\n");
        return 1;
    }
    uintptr_t stack_span = stack_high - stack_low;
    if (stack_span > 64) {
        fprintf(stderr, "lowering probes: O-level copy stack span %" PRIuPTR " bytes\n",
                stack_span);
        return 1;
    }
    printf("lowering probes: one-million 4 KiB copies, stack span %" PRIuPTR " bytes\n",
           stack_span);
    return 0;
}

int main(void)
{
    return run_probes();
}

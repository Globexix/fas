#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

extern uint32_t fas_round_trip(void);
extern uint32_t fas_integer_storage(void);
extern uint32_t fas_byte_split(void);
extern uint32_t fas_low_tag(void);
extern uint32_t fas_excursion(void);
extern uint32_t fas_signed_compound(void);
extern uint32_t fas_unsigned_compound(void);
extern uint32_t fas_reproducer_gep(void);
extern uint32_t fas_reproducer_bits(void);
extern uint32_t fas_mmap_read(uintptr_t pointer);
extern uint32_t fas_foreign_write(void);

static uint32_t *saved_address;

void fas_save_address(void *pointer)
{
    saved_address = (uint32_t *)pointer;
}

void fas_mutate_saved(void)
{
    *saved_address = 37u;
}

static int expect(const char *name, uint32_t got, uint32_t expected)
{
    if (got == expected) return 0;
    fprintf(stderr, "address reconstruction: %s got %u expected %u\n", name, got, expected);
    return 1;
}

int main(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    void *mapping;
    uint32_t expected = UINT32_C(0x91a2b3c4);
    int failed = 0;

    mapping = mmap(NULL, (size_t)page_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED) {
        perror("mmap");
        return 1;
    }
    *(uint32_t *)mapping = expected;

    failed |= expect("addr round trip", fas_round_trip(), 11u);
    failed |= expect("integer storage", fas_integer_storage(), 13u);
    failed |= expect("byte split and rejoin", fas_byte_split(), 17u);
    failed |= expect("low bit tag", fas_low_tag(), 19u);
    failed |= expect("out and back", fas_excursion(), 23u);
    failed |= expect("signed compound offset", fas_signed_compound(), 41u);
    failed |= expect("unsigned compound offset", fas_unsigned_compound(), 43u);
    failed |= expect("p plus difference", fas_reproducer_gep(), 7u);
    failed |= expect("bits plus difference", fas_reproducer_bits(), 7u);
    failed |= expect("external mapping", fas_mmap_read((uintptr_t)mapping), expected);
    failed |= expect("foreign mutation", fas_foreign_write(), 37u);

    if (munmap(mapping, (size_t)page_size) != 0) {
        perror("munmap");
        return 1;
    }
    if (failed) return 1;
    puts("address reconstruction: approved cases: ok");
    return 0;
}

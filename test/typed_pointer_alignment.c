#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

uint8_t *c_allocate_aligned(size_t alignment, size_t size) {
    return aligned_alloc(alignment, size);
}

void c_release_aligned(uint8_t *storage) { free(storage); }

bool c_is_aligned(const uint8_t *storage, size_t alignment) {
    return (uintptr_t)storage % alignment == 0;
}

bool c_check_lanes(const uint64_t *values, size_t count, uint64_t expected) {
    for (size_t index = 0; index < count; ++index) {
        if (values[index] != expected) {
            return false;
        }
    }
    return true;
}

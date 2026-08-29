#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

struct Pair {
    uint64_t left;
    int64_t right;
};

struct __attribute__((aligned(16))) Aligned {
    uint8_t tag;
    uint64_t value;
};

_Static_assert(sizeof(struct Pair) == 16, "Pair size");
_Static_assert(_Alignof(struct Pair) == 8, "Pair alignment");
_Static_assert(offsetof(struct Pair, right) == 8, "Pair field offset");
_Static_assert(sizeof(struct Aligned) == 16, "Aligned size");
_Static_assert(_Alignof(struct Aligned) == 16, "Aligned alignment");
_Static_assert(offsetof(struct Aligned, value) == 8, "Aligned field offset");

bool c_bool(bool x) { return x; }
uint8_t c_u8(uint8_t x) { return x; }
int8_t c_i8(int8_t x) { return x; }
uint16_t c_u16(uint16_t x) { return x; }
int16_t c_i16(int16_t x) { return x; }
uint32_t c_u32(uint32_t x) { return x; }
int32_t c_i32(int32_t x) { return x; }
uint64_t c_u64(uint64_t x) { return x; }
int64_t c_i64(int64_t x) { return x; }
size_t c_usize(size_t x) { return x; }
ptrdiff_t c_isize(ptrdiff_t x) { return x; }
const uint8_t *c_const_ptr(const uint8_t *value) { return value; }
uint8_t *c_mut_ptr(uint8_t *value) { return value; }

uint64_t c_check_pair(const struct Pair *value) {
    return value->left == 41 && value->right == -12;
}

void c_write_pair(struct Pair *value) {
    value->left = 99;
    value->right = -44;
}

uint64_t c_check_aligned(const struct Aligned *value) {
    return (uintptr_t)value % _Alignof(struct Aligned) == 0 && value->tag == 7 &&
           value->value == 1234;
}

uint64_t c_variadic(uint64_t marker, ...) {
    va_list args;
    va_start(args, marker);
    int b = va_arg(args, int);
    int u8_value = va_arg(args, int);
    int i8_value = va_arg(args, int);
    int u16_value = va_arg(args, int);
    int i16_value = va_arg(args, int);
    va_end(args);
    return marker == 77 && b == 1 && u8_value == 200 && i8_value == -100 &&
           u16_value == 50000 && i16_value == -20000;
}

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <limits.h>

struct Token;

extern _Bool asm_echo_bool(_Bool);
extern uint8_t asm_echo_u8(uint8_t);
extern int8_t asm_echo_i8(int8_t);
extern uint16_t asm_echo_u16(uint16_t);
extern int16_t asm_echo_i16(int16_t);
extern uint32_t asm_echo_u32(uint32_t);
extern int32_t asm_echo_i32(int32_t);
extern uint64_t asm_echo_u64(uint64_t);
extern int64_t asm_echo_i64(int64_t);
extern size_t asm_echo_usize(size_t);
extern ptrdiff_t asm_echo_isize(ptrdiff_t);
extern void *asm_echo_addr(void *);
extern struct Token *asm_echo_handle(struct Token *);
extern void asm_noop(void);
extern uint64_t asm_seven(uint8_t, int16_t, uint32_t, int64_t, size_t, _Bool, uint64_t);
extern void asm_write(void *);
extern int32_t fas_verify_assembly(void);

#define CHECK(x) do { if (!(x)) return __LINE__ % 239 + 1; } while (0)

int main(void) {
    CHECK(fas_verify_assembly() == 0);
    CHECK(asm_echo_bool(0) == 0 && asm_echo_bool(1) == 1);
    CHECK(asm_echo_u8(0) == 0 && asm_echo_u8(UINT8_MAX) == UINT8_MAX && asm_echo_u8((uint8_t)-1) == UINT8_MAX);
    CHECK(asm_echo_i8(INT8_MIN) == INT8_MIN && asm_echo_i8(INT8_MAX) == INT8_MAX && asm_echo_i8(-1) == -1);
    CHECK(asm_echo_u16(0) == 0 && asm_echo_u16(UINT16_MAX) == UINT16_MAX && asm_echo_u16((uint16_t)-1) == UINT16_MAX);
    CHECK(asm_echo_i16(INT16_MIN) == INT16_MIN && asm_echo_i16(INT16_MAX) == INT16_MAX && asm_echo_i16(-1) == -1);
    CHECK(asm_echo_u32(0) == 0 && asm_echo_u32(UINT32_MAX) == UINT32_MAX);
    CHECK(asm_echo_i32(INT32_MIN) == INT32_MIN && asm_echo_i32(INT32_MAX) == INT32_MAX && asm_echo_i32(-1) == -1);
    CHECK(asm_echo_u64(0) == 0 && asm_echo_u64(UINT64_MAX) == UINT64_MAX);
    CHECK(asm_echo_i64(INT64_MIN) == INT64_MIN && asm_echo_i64(INT64_MAX) == INT64_MAX && asm_echo_i64(-1) == -1);
    CHECK(asm_echo_usize(0) == 0 && asm_echo_usize(SIZE_MAX) == SIZE_MAX);
    CHECK(asm_echo_isize(PTRDIFF_MIN) == PTRDIFF_MIN && asm_echo_isize(PTRDIFF_MAX) == PTRDIFF_MAX);
    uint8_t byte = 0;
    CHECK(asm_echo_addr(NULL) == NULL && asm_echo_addr(&byte) == &byte);
    CHECK(asm_echo_handle(NULL) == NULL && asm_echo_handle((struct Token *)&byte) == (struct Token *)&byte);
    uint64_t expected = UINT8_MAX ^ (uint64_t)(int64_t)-2 ^ (uint64_t)UINT32_MAX ^ (uint64_t)INT64_MIN ^ SIZE_MAX ^ 1 ^ UINT64_MAX;
    CHECK(asm_seven(UINT8_MAX, -2, UINT32_MAX, INT64_MIN, SIZE_MAX, 1, UINT64_MAX) == expected);
    asm_noop();
    asm_write(&byte);
    CHECK(byte == 73);
    return 0;
}

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

bool fas_bool(bool x);
uint8_t fas_u8(uint8_t x);
int8_t fas_i8(int8_t x);
uint16_t fas_u16(uint16_t x);
int16_t fas_i16(int16_t x);
uint32_t fas_u32(uint32_t x);
int32_t fas_i32(int32_t x);
uint64_t fas_u64(uint64_t x);
int64_t fas_i64(int64_t x);
size_t fas_usize(size_t x);
ptrdiff_t fas_isize(ptrdiff_t x);
uint8_t fas_read(const uint8_t *value);
void fas_write(uint8_t *value, uint8_t replacement);
const uint8_t *fas_const_ptr(const uint8_t *value);
uint8_t *fas_mut_ptr(uint8_t *value);
void fas_noop(void);

int main(void) {
  if (!fas_bool(true) || fas_bool(false))
    return 1;
  if (fas_u8(UINT8_MAX) != UINT8_MAX)
    return 2;
  if (fas_i8(INT8_MIN) != INT8_MIN || fas_i8(INT8_MAX) != INT8_MAX)
    return 3;
  if (fas_u16(UINT16_MAX) != UINT16_MAX)
    return 4;
  if (fas_i16(INT16_MIN) != INT16_MIN || fas_i16(INT16_MAX) != INT16_MAX)
    return 5;
  if (fas_u32(UINT32_MAX) != UINT32_MAX)
    return 6;
  if (fas_i32(INT32_MIN) != INT32_MIN || fas_i32(INT32_MAX) != INT32_MAX)
    return 7;
  if (fas_u64(UINT64_MAX) != UINT64_MAX)
    return 8;
  if (fas_i64(INT64_MIN) != INT64_MIN || fas_i64(INT64_MAX) != INT64_MAX)
    return 9;
  if (fas_usize(SIZE_MAX) != SIZE_MAX)
    return 10;
  if (fas_isize(PTRDIFF_MIN) != PTRDIFF_MIN ||
      fas_isize(PTRDIFF_MAX) != PTRDIFF_MAX)
    return 11;
  const uint8_t source[] = {231};
  if (fas_read(source) != 231)
    return 12;
  if (fas_const_ptr(source) != source)
    return 13;
  uint8_t destination[] = {0};
  fas_write(destination, 197);
  if (destination[0] != 197)
    return 14;
  if (fas_mut_ptr(destination) != destination)
    return 15;
  fas_noop();
  return 0;
}

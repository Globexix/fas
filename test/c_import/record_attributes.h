#include <stddef.h>
#include <stdint.h>

struct FasPackedTrailing {
  uint32_t prefix;
  uint64_t value;
} __attribute__((packed));

struct FasAlignedTrailing {
  uint32_t value;
} __attribute__((aligned(16)));

typedef struct {
  unsigned char bytes[32];
} FasAlignedTypedef __attribute__((aligned(32)));

enum __attribute__((aligned(8))) FasAlignedEnum {
  FasAlignedEnumZero = 0,
  FasAlignedEnumOne = 1
};

static inline size_t fas_packed_size(void) { return sizeof(struct FasPackedTrailing); }
static inline size_t fas_packed_align(void) { return _Alignof(struct FasPackedTrailing); }
static inline size_t fas_packed_value_offset(void) {
  return offsetof(struct FasPackedTrailing, value);
}
static inline size_t fas_aligned_size(void) { return sizeof(struct FasAlignedTrailing); }
static inline size_t fas_aligned_align(void) { return _Alignof(struct FasAlignedTrailing); }
static inline size_t fas_aligned_value_offset(void) {
  return offsetof(struct FasAlignedTrailing, value);
}
static inline size_t fas_typedef_size(void) { return sizeof(FasAlignedTypedef); }
static inline size_t fas_typedef_align(void) { return _Alignof(FasAlignedTypedef); }
static inline size_t fas_typedef_bytes_offset(void) {
  return offsetof(FasAlignedTypedef, bytes);
}

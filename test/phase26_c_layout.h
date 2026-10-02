#include <stdint.h>
#include <stddef.h>

struct FasDeepMin {
  uint8_t a;
  struct {
    uint16_t b;
    struct { uint32_t x; };
  };
  uint64_t z;
};

enum __attribute__((packed)) FasByteEnum {
  FasByte0 = 0,
  FasByteMax = 255
};

struct __attribute__((packed, aligned(4))) FasPackedStorage {
  uint8_t byte;
  uint32_t word;
  double ignored;
};

extern enum FasByteEnum fas_enum_values[4];
extern struct FasPackedStorage fas_packed_storage;

void fas_seed_deep(struct FasDeepMin *value);
uint32_t fas_read_deep_x(struct FasDeepMin *value);
size_t fas_c_deep_size(void);
size_t fas_c_deep_align(void);
size_t fas_c_deep_x_offset(void);
uint32_t fas_c_enum_sum(void);
size_t fas_c_packed_size(void);
size_t fas_c_packed_align(void);
size_t fas_c_packed_word_offset(void);

#include "generic_const_arguments_oracle.h"
#include <stdint.h>

typedef struct {
  uint16_t value;
} Box16;

typedef struct {
  uint32_t value;
} Box32;

typedef struct {
  uint8_t data[sizeof(uint16_t)];
} Direct16;

typedef struct {
  uint8_t data[sizeof(uint16_t) * 2];
} Doubled16;

typedef struct {
  uint8_t data[sizeof(Box16)];
} Wrapped16;

typedef struct {
  Direct16 direct;
  Doubled16 doubled;
  Wrapped16 wrapped;
  uint8_t nested[sizeof(Wrapped16)];
} Matrix16;

typedef struct {
  uint8_t data[sizeof(uint32_t)];
} Direct32;

typedef struct {
  uint8_t data[sizeof(uint32_t) * 2];
} Doubled32;

typedef struct {
  uint8_t data[sizeof(Box32)];
} Wrapped32;

typedef struct {
  Direct32 direct;
  Doubled32 doubled;
  Wrapped32 wrapped;
  uint8_t nested[sizeof(Wrapped32)];
} Matrix32;

size_t oracle_probe_u16(void) {
  return sizeof(Direct16) + sizeof(Doubled16) * 10 + sizeof(Wrapped16) * 100
         + sizeof(Wrapped16) * 1000 + sizeof(Matrix16) * 10000;
}

size_t oracle_probe_u32(void) {
  return sizeof(Direct32) + sizeof(Doubled32) * 10 + sizeof(Wrapped32) * 100
         + sizeof(Wrapped32) * 1000 + sizeof(Matrix32) * 10000;
}

size_t oracle_matrix_u16(void) { return sizeof(Matrix16); }
size_t oracle_matrix_u32(void) { return sizeof(Matrix32); }

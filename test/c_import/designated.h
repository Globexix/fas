#include <stdint.h>

typedef struct FasImported {
    uint8_t tag;
    uint32_t value;
    uint16_t tail;
} FasImported;

typedef union FasChoice {
    int32_t value;
} FasChoice;

int32_t c_check_outer(const void *actual, int32_t which);
int32_t c_check_envelope(const void *actual);
int32_t c_check_imported(const void *actual);

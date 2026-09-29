#include <stdint.h>

extern uint64_t fas_library_value(void);

int main(void) {
    return fas_library_value() == UINT64_C(81985529216486895) ? 0 : 1;
}

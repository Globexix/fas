#include "designated_order.h"

static int32_t events[16];
static int32_t event_count;

void clear_order(void) {
    event_count = 0;
}

int32_t order_event(int32_t value) {
    events[event_count++] = value;
    return value;
}

int32_t order_matches(const int32_t *expected, int32_t count) {
    if (event_count != count) return 1;
    for (int32_t index = 0; index < count; ++index) {
        if (events[index] != expected[index]) return 1;
    }
    return 0;
}

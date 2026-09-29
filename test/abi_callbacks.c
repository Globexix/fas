#include <stddef.h>
#include <stdint.h>

typedef uint32_t (*callback_fn)(void *, uint32_t);

extern uint32_t callback(void *user, uint32_t value);
extern uint32_t reentrant_callback(void *user, uint32_t value);
extern uint32_t fas_resume(void *user, uint32_t value);

uint32_t c_reenter(void *user, uint32_t value) {
    return fas_resume(user, value) + 1;
}

static uint64_t apply(callback_fn callback, void *user, const uint32_t *values, size_t count) {
    uint64_t total = 0;
    for (size_t i = 0; i < count; ++i) total += callback(user, values[i]);
    return total;
}

int main(void) {
    uint32_t bias = 5;
    const uint32_t values[] = {1, 2, 3, 7, 11};
    if (apply(callback, &bias, values, sizeof(values) / sizeof(values[0])) != 49) return 1;
    if (apply(reentrant_callback, &bias, values, sizeof(values) / sizeof(values[0])) != 78) return 2;
    return 0;
}

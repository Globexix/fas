#include <stdint.h>
#include <stdio.h>

static int byte_class(uint8_t value) {
    switch (value) {
    case 0:
    case '\n':
    case '\r':
    case '\t':
        return 1;
    case 'a':
    case 'e':
    case 'i':
    case 'o':
    case 'u':
        return 2;
    default:
        return 0;
    }
}

static int signed_class(int32_t value) {
    switch (value) {
    case -9:
    case -1:
        return 1;
    case 0:
    case 1:
        return 2;
    default:
        return 0;
    }
}

static int wide_class(uint64_t value) {
    switch (value) {
    case UINT64_C(4294967296):
    case UINT64_C(4294967297):
        return 1;
    default:
        return 0;
    }
}

static int enum_class(int32_t value) {
    enum Color { Red = 3, Blue = 8 };
    switch (value) {
    case Red:
    case Blue:
        return 1;
    default:
        return 0;
    }
}

static int generic_class(uint32_t limit, uint32_t value) {
    switch (value) {
    case 20:
    case 21:
        return value == limit || value == limit + 1;
    default:
        return 0;
    }
}

static uint64_t break_two(void) {
    uint64_t trace = 0;
    for (int32_t i = 0; i < 1; ++i) {
        for (int32_t j = 0; j < 1; ++j) {
            switch (0) {
            case 0:
                trace = trace * 10 + 3;
                goto done;
            default:
                break;
            }
        }
    }
done:
    trace = trace * 10 + 2;
    trace = trace * 10 + 1;
    return trace;
}

static uint64_t break_three(void) {
    uint64_t trace = 0;
    trace = trace * 10 + 3;
    trace = trace * 10 + 2;
    trace = trace * 10 + 1;
    return trace;
}

static uint64_t continue_outer(void) {
    uint64_t trace = 0;
    for (int32_t i = 0; i < 3; ++i) {
        for (int32_t j = 0; j < 2; ++j) {
            switch (i) {
            case 0:
            case 1:
            case 2:
                trace = trace * 10 + 3;
                trace = trace * 10 + 2;
                trace = trace * 10 + 1;
                goto next_outer;
            default:
                break;
            }
        }
next_outer:
        continue;
    }
    return trace;
}

int main(void) {
    printf("%d %d %d\n", byte_class(10), byte_class('e'), byte_class('x'));
    printf("%d %d %d\n", signed_class(-9), signed_class(1), signed_class(7));
    printf("%d %d\n", wide_class(UINT64_C(4294967296)), wide_class(UINT64_C(4294967298)));
    printf("%d %d %d %d\n", enum_class(3), enum_class(8), enum_class(4), generic_class(20, 20));
    printf("%llu %llu %llu\n", (unsigned long long)break_two(),
           (unsigned long long)break_three(), (unsigned long long)continue_outer());
    return 0;
}

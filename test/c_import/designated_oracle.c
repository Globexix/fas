#include "designated.h"

#include <stddef.h>
#include <string.h>

struct Inner {
    uint8_t first;
    uint32_t second;
    uint16_t third;
};

struct Outer {
    uint8_t prefix;
    struct Inner inner;
    struct Inner items[2];
    uint64_t tail;
};

struct Envelope {
    uint16_t marker;
    struct Outer outer;
};

struct ExportedOuter {
    uint8_t prefix;
    struct Inner inner;
    struct Inner items[2];
    uint64_t tail;
};

extern struct ExportedOuter exported_value;

_Static_assert(sizeof(struct Inner) == 12, "Inner size");
_Static_assert(sizeof(struct Outer) == 48, "Outer size");
_Static_assert(offsetof(struct Outer, inner) == 4, "Outer inner offset");
_Static_assert(offsetof(struct Outer, items) == 16, "Outer items offset");
_Static_assert(offsetof(struct Outer, tail) == 40, "Outer tail offset");
_Static_assert(sizeof(struct Envelope) == 56, "Envelope size");
_Static_assert(offsetof(struct Envelope, outer) == 8, "Envelope outer offset");
_Static_assert(sizeof(FasImported) == 12, "imported size");
_Static_assert(offsetof(FasImported, value) == 4, "imported value offset");
_Static_assert(offsetof(FasImported, tail) == 8, "imported tail offset");

int32_t c_check_outer(const void *actual, int32_t which) {
    struct Outer expected;
    memset(&expected, 0, sizeof expected);
    if (which == 0) {
        expected.prefix = 3;
        expected.inner.first = 5;
        expected.inner.third = 7;
        expected.items[0].second = 11;
        expected.items[1].first = 13;
        expected.items[1].second = 17;
        expected.items[1].third = 19;
        expected.tail = 23;
    } else if (which == 1) {
        expected.prefix = 37;
        expected.inner.first = 38;
        expected.inner.second = 39;
        expected.items[0].third = 43;
        expected.items[1].first = 41;
        expected.items[1].second = 42;
        expected.tail = 47;
    } else if (which == 2) {
        expected.prefix = 53;
        expected.items[0].second = 57;
        expected.items[1].third = 61;
        expected.tail = 59;
    } else {
        expected.prefix = 83;
        expected.inner.third = 79;
        expected.items[0].first = 67;
        expected.items[1].second = 71;
        expected.tail = 73;
    }
    return memcmp(actual, &expected, sizeof expected) == 0 ? 0 : 1;
}

int32_t c_check_imported(const void *actual) {
    FasImported expected;
    memset(&expected, 0, sizeof expected);
    expected.tag = 103;
    expected.tail = 101;
    return memcmp(actual, &expected, sizeof expected) == 0 ? 0 : 1;
}

int32_t c_check_envelope(const void *actual) {
    struct Envelope expected;
    memset(&expected, 0, sizeof expected);
    expected.marker = 97;
    expected.outer.items[0].third = 89;
    return memcmp(actual, &expected, sizeof expected) == 0 ? 0 : 1;
}

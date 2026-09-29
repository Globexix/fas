#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define SLOT_COUNT 7
#define STORAGE_BYTES 32

static uint8_t storage[6][SLOT_COUNT][STORAGE_BYTES];
static const size_t lane_counts[SLOT_COUNT] = {1, 3, 5, 7, 9, 13, 255};

void *mask_storage_buffer(uint32_t category, uint32_t slot) {
    return storage[category][slot];
}

void mask_storage_capture(void *dst, void *src, size_t length) {
    memcpy(dst, src, length);
}

void mask_storage_run_1(void);
void mask_storage_run_3(void);
void mask_storage_run_5(void);
void mask_storage_run_7(void);
void mask_storage_run_9(void);
void mask_storage_run_13(void);
void mask_storage_run_255(void);

static uint8_t expected_byte(size_t lanes, size_t byte_index) {
    uint8_t expected = 0;
    for (size_t bit = 0; bit < 8; ++bit) {
        size_t lane = byte_index * 8 + bit;
        if (lane < lanes)
            expected |= (uint8_t)(1u << bit);
    }
    return expected;
}

static int check_category(size_t category, size_t slot, size_t lanes) {
    size_t bytes = (lanes + 7) / 8;
    for (size_t i = 0; i < STORAGE_BYTES; ++i) {
        uint8_t expected = i < bytes ? expected_byte(lanes, i) : 0xa5;
        if (storage[category][slot][i] != expected) {
            fprintf(stderr,
                    "mask storage: category %zu lanes %zu byte %zu: expected 0x%02x, got 0x%02x\n",
                    category, lanes, i, expected, storage[category][slot][i]);
            return 0;
        }
    }
    return 1;
}

int main(void) {
    void (*run[SLOT_COUNT])(void) = {
        mask_storage_run_1, mask_storage_run_3, mask_storage_run_5,
        mask_storage_run_7, mask_storage_run_9, mask_storage_run_13,
        mask_storage_run_255,
    };
    for (size_t slot = 0; slot < SLOT_COUNT; ++slot) {
        memset(storage, 0xa5, sizeof(storage));
        memset(storage[3][slot], 0xff, STORAGE_BYTES);
        run[slot]();
        if (!check_category(0, slot, lane_counts[slot]) ||
            !check_category(1, slot, lane_counts[slot]) ||
            !check_category(2, slot, lane_counts[slot]) ||
            !check_category(4, slot, lane_counts[slot]) ||
            !check_category(5, slot, lane_counts[slot]))
            return 1;
        for (size_t i = 0; i < STORAGE_BYTES; ++i) {
            if (storage[3][slot][i] != 0xff) {
                fprintf(stderr, "mask storage: copy changed source at lane count %zu\n",
                        lane_counts[slot]);
                return 2;
            }
        }
    }
    return 0;
}

#include <stdint.h>
#include <stdio.h>
#include <string.h>

extern uint64_t fas_ordinary_payload_length(void);
extern uint64_t fas_utf8_payload_length(void);
extern uint64_t fas_c_payload_length(void);
extern const unsigned char *fas_hex_ordinary_bytes(void);
extern uint64_t fas_hex_ordinary_length(void);
extern const unsigned char *fas_hex_c_bytes(void);
extern uint64_t fas_hex_c_length(void);
extern const unsigned char *fas_hex_case_bytes(void);
extern const unsigned char *fas_hex_fixed_width_bytes(void);
extern uint64_t fas_hex_fixed_width_length(void);

int main(void)
{
    static const unsigned char ordinary[] = "a\0b";
    static const unsigned char utf8[] = "\xc3\xa9";
    static const unsigned char c_payload[] = "fas";
    static const unsigned char hex_ordinary[] = {'A', 0, 'B'};
    static const unsigned char hex_c[] = {'A', 'B', 0};
    static const unsigned char hex_case[] = {0x4a, 0xab};
    static const unsigned char hex_fixed_width[] = {'A', '4'};
    uint64_t ordinary_expected = sizeof(ordinary) - 1u;
    uint64_t utf8_expected = sizeof(utf8) - 1u;
    uint64_t c_expected = sizeof(c_payload) - 1u;

    if (fas_ordinary_payload_length() != ordinary_expected) {
        fputs("constants and literals: ordinary payload length mismatch\n", stderr);
        return 1;
    }
    if (fas_utf8_payload_length() != utf8_expected) {
        fputs("constants and literals: UTF-8 payload length mismatch\n", stderr);
        return 1;
    }
    if (fas_c_payload_length() != c_expected) {
        fputs("constants and literals: C payload length mismatch\n", stderr);
        return 1;
    }
    if (fas_hex_ordinary_length() != sizeof(hex_ordinary) ||
        memcmp(fas_hex_ordinary_bytes(), hex_ordinary, sizeof(hex_ordinary)) != 0) {
        fputs("constants and literals: hexadecimal byte string mismatch\n", stderr);
        return 1;
    }
    if (fas_hex_c_length() != 2u ||
        memcmp(fas_hex_c_bytes(), hex_c, sizeof(hex_c)) != 0 ||
        fas_hex_c_bytes()[sizeof(hex_c) - 1u] != 0) {
        fputs("constants and literals: hexadecimal C string mismatch\n", stderr);
        return 1;
    }
    if (memcmp(fas_hex_case_bytes(), hex_case, sizeof(hex_case)) != 0) {
        fputs("constants and literals: hexadecimal case mismatch\n", stderr);
        return 1;
    }
    if (fas_hex_fixed_width_length() != sizeof(hex_fixed_width) ||
        memcmp(fas_hex_fixed_width_bytes(), hex_fixed_width, sizeof(hex_fixed_width)) != 0) {
        fputs("constants and literals: hexadecimal escape width mismatch\n", stderr);
        return 1;
    }

    puts("constants and literals: static constants and payload lengths: ok");
    return 0;
}

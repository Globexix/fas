#include <stdint.h>
#include <stdio.h>

extern uint64_t fas_ordinary_payload_length(void);
extern uint64_t fas_utf8_payload_length(void);
extern uint64_t fas_c_payload_length(void);

int main(void)
{
    static const unsigned char ordinary[] = "a\0b";
    static const unsigned char utf8[] = "\xc3\xa9";
    static const unsigned char c_payload[] = "fas";
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

    puts("constants and literals: static constants and payload lengths: ok");
    return 0;
}

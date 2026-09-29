#include <stdint.h>
#include <stdio.h>
#include <string.h>

static const uint8_t character_bytes[102] = {
    '\n', '\r', '\t', '\\', '\'', '\"', '\0', '\x41', '\xFF',
    ' ', '!', '"', '#', '$', '%', '&', '(', ')',
    '*', '+', ',', '-', '.', '/', '0', '1', '2',
    '3', '4', '5', '6', '7', '8', '9', ':', ';',
    '<', '=', '>', '?', '@', 'A', 'B', 'C', 'D',
    'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
    'W', 'X', 'Y', 'Z', '[', ']', '^', '_', '`',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{',
    '|', '}', '~'
};
static const uint8_t character_const = 'a';

int main(void) {
    for (size_t i = 0; i < 102; ++i) printf("%u\n", (unsigned)character_bytes[i]);
    uint8_t a = 'a';
    int8_t b = 'a';
    int32_t c = 'a';
    uint64_t d = 'a';
    size_t e = 'a';
    uint8_t values[1] = {'a'};
    printf("%u\n", (unsigned)a);
    printf("%u\n", (unsigned)(unsigned char)b);
    printf("%d\n", c);
    printf("%u\n", (unsigned)d);
    printf("%u\n", (unsigned)e);
    printf("%u\n", (unsigned)character_const);
    switch (a) { case 'a': printf("1\n"); break; default: printf("0\n"); }
    printf("%u\n", (unsigned)values[0]);
    printf("%u\n", (unsigned)(values[0] == 'a'));
    printf("%d\n", -'a');
    printf("%u\n", (uint8_t)'\xFF');
    const unsigned char text[] = "Aé'Z";
    for (size_t i = 0; i < 5; ++i) printf("%u\n", (unsigned)text[i]);
    uint32_t word;
    memcpy(&word, text, sizeof(word));
    printf("%u\n", word);
    return 0;
}

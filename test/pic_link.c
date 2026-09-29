#include <stdint.h>
#include <stdio.h>
#include <string.h>

uint32_t pic_global = 17;

static const uint32_t oracle_values[] = {2, 5, 11, 13};

extern uint32_t fas_pic_probe(size_t index);

#ifdef PIC_ASM
extern uint32_t fas_pic_add(uint32_t delta);
#endif

uint32_t pic_adjust(uint32_t delta)
{
    pic_global += delta;
    return pic_global;
}

int32_t pic_string_ok(const char *value)
{
    return strcmp(value, "fas-pic") == 0;
}

int main(void)
{
    uint32_t initial = pic_global;
    uint32_t expected = initial + oracle_values[2];
    uint32_t actual = fas_pic_probe(2);
    if (actual != expected || pic_global != expected) return 1;

#ifdef PIC_ASM
    expected += 3;
    actual = fas_pic_add(3);
    if (actual != expected || pic_global != expected) return 2;
#endif

    puts("pic-link: ok");
    return 0;
}

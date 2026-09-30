#include <stdio.h>
#include <string.h>

int Imported = 17;
char names[4][8];
struct Record { int head; int body[3]; };
struct Entry { const char *name; const struct Entry *child; const struct Entry *parent; };

int inspect_tables(void *raw_slots, void *raw_menu, void *raw_sprnames) {
    void *const *slots = raw_slots;
    const struct Entry *menu = raw_menu;
    const char *const *sprnames = raw_sprnames;
    if (strcmp(sprnames[0], "TROO") || strcmp(sprnames[1], "SARG") || strcmp(sprnames[2], "PLAY")) return 1;
    if (strcmp(menu[0].name, "root") || strcmp(menu[1].name, "leaf")) return 2;
    if (menu[0].child != &menu[1] || menu[0].parent != menu || menu[1].child || menu[1].parent != menu) return 3;
    struct Record *target = slots[0];
    if (slots[1] != &target->head || slots[2] != &target->body[1] || slots[3] != &Imported) return 4;
    target->body[0] = 103;
    *(int *)slots[1] = 101;
    *(int *)slots[2] = 107;
    *(int *)slots[3] = 109;
    names[2][3] = 'Z';
    return 0;
}

#ifndef ADDRESS_ORACLE
extern int fas_run(void);
int main(void) {
    int result = fas_run();
    if (result || names[1][4] != 'Q') return result ? result : 11;
    printf("%d %d %d %d\n", Imported, names[2][3], names[1][4], result);
    return 0;
}
#endif

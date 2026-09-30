#include <stdio.h>

extern int Imported;
extern char names[4][8];
extern int inspect_tables(void *, void *, void *);
struct Record { int head; int body[3]; };
struct Entry { const char *name; const struct Entry *child; const struct Entry *parent; };
static struct Record Target = {5, {7, 11, 13}};
static const char *const Sprnames[3] = {"TROO", "SARG", "PLAY"};
static const struct Entry Menu[2] = {{"root", &Menu[1], &Menu[0]}, {"leaf", 0, &Menu[0]}};
static void *const Slots[4] = {&Target, &Target.head, &Target.body[1], &Imported};
static void *B;
static void *A = &B;
static void *B = &A;
int main(void) {
    if (Sprnames[0][0] != 'T' || Sprnames[2][3] != 'Y') return 1;
    if (Menu[0].child != &Menu[1] || Menu[1].parent != &Menu[0]) return 2;
    if (A != &B || B != &A) return 3;
    if (inspect_tables((void *)Slots, (void *)Menu, (void *)Sprnames)) return 4;
    if (Target.head != 101 || Target.body[0] != 103 || Target.body[1] != 107 || Imported != 109) return 5;
    if (names[2][3] != 'Z') return 6;
    names[1][4] = 'Q';
    if (*(int *)Slots[2] != 107) return 7;
    printf("%d %d %d %d\n", Imported, names[2][3], names[1][4], 0);
    return 0;
}

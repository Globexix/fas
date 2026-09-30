#include "api.h"
#include <stdlib.h>
int32_t fas_vec_3_u8 = 5;
int32_t scalar = 7;
int32_t table[4] = {4,-2,9,1};
struct State state = {{3,true},{2,2,2},{true,true,true,true,true,true,true,true},NULL,NULL,{{1,2,3},{4,5,6}}};
void *table_address = table;
void *nested_address = &state.nested;
void *text = "fas";
struct ExportTag *tagged;
ExportAlias *alias;
int32_t dependency_value(void) { return 11; }
int32_t fas_compare(void *a, void *b) {
    int32_t x = *(int32_t *)a;
    int32_t y = *(int32_t *)b;
    return (x > y) - (x < y);
}
void fas_event(int32_t value, void *data) { *(int32_t *)data += value * scalar; }
int32_t fas_read(void) { return scalar + table[0] + state.data[1][2] + state.vector[2]; }
int32_t fas_inline(int32_t x) { return x + 5; }
struct ExportTag *fas_tag(struct ExportTag *x) { return x; }
ExportAlias *fas_alias(ExportAlias *x) { return x; }
bool fas_bool(bool x) { return !x; }
static int compare_for_qsort(const void *a, const void *b) {
    return fas_compare((void *)a, (void *)b);
}
int sort_values(void *base, size_t count) {
    qsort(base, count, sizeof(int32_t), compare_for_qsort);
    return 11;
}
int run_events(void *data) {
    void (*callback)(int32_t, void *) = fas_event;
    for (int i = 1; i <= 4; ++i) callback(i, data);
    return fas_read();
}

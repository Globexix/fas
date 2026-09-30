#include "api.h"
#include <stdio.h>
#include <string.h>
int sort_values(void *base, size_t count);
int run_events(void *data);
int main(void) {
    if (fas_vec_3_u8 != 5) return 7;
    struct ExportTag tag = {19};
    ExportAlias opaque_alias = {23};
    if (fas_tag(&tag) != &tag || fas_alias(&opaque_alias) != &opaque_alias) return 1;
    if (tagged != NULL || alias != NULL || state.token != NULL) return 2;
    if (table_address != (void *)table || nested_address != (void *)&state.nested) return 3;
    if (strcmp((const char *)text, "fas") != 0 || fas_bool(true) || !fas_bool(false)) return 4;
    if (state.nested.x != 3 || !state.nested.flag || state.vector[2] != 2 || !state.mask[5]) return 5;
    printf("initial %d %d %d %u %d\n", scalar, table[2], state.data[1][2],
           (unsigned)state.vector[2], fas_read());
    scalar = 9;
    state.nested.x = -12;
    state.nested.flag = false;
    state.vector[2] = 17;
    state.mask[5] = false;
    state.pointer = &scalar;
    state.data[1][2] = 40;
    tagged = &tag;
    alias = &opaque_alias;
    int32_t result = sort_values(table, 4);
    int32_t events = 2;
    int32_t after = run_events(&events);
    printf("sorted %d %d %d %d dep %d\n", table[0], table[1], table[2], table[3], result);
    printf("events %d read %d inline %d fields %d %d %d\n", events, after, fas_inline(6),
           state.nested.x, state.nested.flag, state.mask[5]);
    if (state.pointer != &scalar || tagged != &tag || alias != &opaque_alias) return 6;
    return 0;
}

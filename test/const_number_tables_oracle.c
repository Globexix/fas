#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

void dump_i64(long value);
void dump_u64(unsigned long value);
void dump_f64(double value);
void dump_bytes(int label, const void *bytes, unsigned long count);

struct Cell { uint8_t tag; uint32_t weight; double scale; bool live; uint16_t step; };
struct Row { uint8_t id; struct Cell cells[2]; };
struct Pair { uint8_t lo; uint16_t hi; };

static const struct Cell CELLS[3] = {{1, 100, 1.5, true, 7}, {2, 200, 2.25, false, 9}, {255, 4000000000u, -0.5, true, 65535}};
static const int32_t Grid[2][3] = {{1, -2, 3}, {4, 5, -6}};
static const uint16_t Cube[2][2][2] = {{{1, 2}, {3, 4}}, {{5, 6}, {7, 65535}}};
static const struct Row Rows[2] = {{9, {{1, 11, 2.0, true, 10}, {3, 33, 4.0, false, 20}}}, {8, {{5, 55, 6.0, true, 30}, {7, 77, 8.0, true, 40}}}};
static const struct Pair Pairs[3] = {{1, 2}, {3, 4}, {5, 65535}};
static const struct Cell Empty[3] = {{0}};
static const struct Cell One = {42, 4242, 0.25, false, 1};
static const struct Cell Named = {4, 5, 1.0, true, 3};

static int dump_cell(const struct Cell *c) {
    dump_i64(c->tag);
    dump_i64(c->weight);
    dump_f64(c->scale);
    dump_i64(c->live);
    dump_i64(c->step);
    return 0;
}

static int oracle_run(void) {
    unsigned long total = 0;
    for (size_t i = 0; i < 3; i++) {
        dump_cell(&CELLS[i]);
        total += CELLS[i].weight;
    }
    dump_u64(total);
    for (size_t r = 0; r < 2; r++)
        for (size_t c = 0; c < 3; c++) dump_i64(Grid[r][c]);
    for (size_t a = 0; a < 2; a++)
        for (size_t b = 0; b < 2; b++)
            for (size_t c = 0; c < 2; c++) dump_i64(Cube[a][b][c]);
    for (size_t r = 0; r < 2; r++) {
        dump_i64(Rows[r].id);
        for (size_t k = 0; k < 2; k++) dump_cell(&Rows[r].cells[k]);
    }
    dump_i64(Grid[1][2]);
    dump_cell(&Rows[1].cells[1]);
    for (size_t p = 0; p < 3; p++) {
        dump_i64(Pairs[p].lo);
        dump_i64(Pairs[p].hi);
    }
    dump_cell(&One);
    dump_cell(&Named);
    total = 0;
    for (size_t e = 0; e < 3; e++) total += Empty[e].weight;
    dump_u64(total);
    dump_bytes(0, CELLS, sizeof CELLS);
    dump_bytes(1, Grid, sizeof Grid);
    dump_bytes(2, Cube, sizeof Cube);
    dump_bytes(3, Rows, sizeof Rows);
    dump_bytes(4, Pairs, sizeof Pairs);
    dump_bytes(5, Empty, sizeof Empty);
    dump_bytes(6, &One, sizeof One);
    dump_bytes(7, &Named, sizeof Named);
    return 0;
}

int main(void) {
    int result = oracle_run();
    printf("result %d\n", result);
    return result;
}

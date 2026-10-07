#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>

typedef struct Object Object;
typedef struct State State;
typedef void (*object_fn)(Object *);
struct State { object_fn think; object_fn action; const State *next; };
struct Object { const State *state; int32_t id; int32_t ticks; int32_t total; };
typedef struct FasCallbackBox { int32_t (*callback)(int32_t); } FasCallbackBox;

static int32_t fas_box_target(int32_t value) { return value * 4; }
static FasCallbackBox fas_callback_box = {fas_box_target};
static inline int32_t fas_inline_adapter(int32_t value) { return value + 9; }
static int32_t fas_callback_i32(int32_t value) { return value + 7; }
static int8_t fas_callback_i8(int8_t value) { return (int8_t)(value + 1); }
static bool fas_callback_bool(bool value) { return !value; }
static int32_t fas_c_invoke_i32(int32_t (*callback)(int32_t), int32_t value) { return callback(value); }
static int32_t fas_c_invoke_i8(int8_t (*callback)(int8_t), int8_t value) { return callback(value); }
static bool fas_c_invoke_bool(bool (*callback)(bool), bool value) { return callback(value); }
static int32_t zero_args(void) { return 17; }
static int32_t one_arg(int32_t value) { return value * 3 + 2; }
static int32_t three_args(int32_t first, int32_t second, int32_t third) { return first + second * 2 + third * 3; }
static int32_t address_only(int32_t value) { return value * 5 - 1; }
static int32_t recursive(int32_t value) { return value <= 0 ? 1 : value * recursive(value - 1); }
static int8_t fas_export_i8(int8_t value) { return (int8_t)(value + 2); }
static int32_t evaluation_order;
static int32_t order_target(int32_t first, int32_t second) { return first + second; }
static int32_t order_first(void) { evaluation_order = evaluation_order * 10 + 2; return 2; }
static int32_t order_second(void) { evaluation_order = evaluation_order * 10 + 3; return 3; }

static void think0(Object *object) { if (++object->ticks % 3 == 0) object->state = object->state->next; }
static void think1(Object *object) { if (++object->ticks % 4 == 0) object->state = object->state->next; }
static void think2(Object *object) { if (++object->ticks % 5 == 0) object->state = object->state->next; }
static void think3(Object *object) { if (++object->ticks % 6 == 0) object->state = object->state->next; }
static void action0(Object *object) { object->total += object->id + 1; }
static void action1(Object *object) { object->total += (object->id + 1) * 2; }
static void action2(Object *object) { object->total += (object->id + 1) * 3; }
static void action3(Object *object) { object->total += (object->id + 1) * 4; }
static const State states[4];
static const State states[4] = {
    {think0, action0, &states[1]},
    {think1, action1, &states[2]},
    {think2, action2, &states[3]},
    {think3, action3, &states[0]},
};

int main(void) {
    Object objects[3] = {{&states[0], 0, 0, 0}, {&states[1], 1, 0, 0}, {&states[3], 2, 0, 0}};
    for (int32_t i = 0; i < 20; ++i) {
        for (int32_t j = 0; j < 3; ++j) {
            objects[j].state->think(&objects[j]);
            objects[j].state->action(&objects[j]);
        }
    }
    objects[0].total += 100;

    int32_t calls = zero_args();
    calls += one_arg(7);
    calls += three_args(2, 3, 5);
    calls += address_only(8);
    calls += one_arg(9);
    calls += one_arg(11);
    calls += fas_c_invoke_i32(fas_callback_i32, 6);
    calls += fas_callback_box.callback(5);
    calls += fas_inline_adapter(8);
    calls += three_args(1, 2, 3);
    calls += recursive(5);
    calls += fas_c_invoke_i8(fas_callback_i8, (int8_t)249);
    calls += fas_c_invoke_bool(fas_callback_bool, false);
    calls += fas_export_i8((int8_t)40);
    evaluation_order = 1;
    int32_t first_arg = order_first();
    int32_t second_arg = order_second();
    int32_t ordered = order_target(first_arg, second_arg);
    if (evaluation_order != 123 || ordered != 5) return 5;

    int32_t score = 0;
    score += (int32_t)(bool)true;
    score += (int32_t)(uint8_t)250;
    score += (int32_t)(int8_t)249;
    score += (int32_t)(uint16_t)60000;
    score += (int32_t)(int16_t)64302;
    score += (int32_t)(uint32_t)70000;
    score += (int32_t)-70000;
    score += (int32_t)(uint64_t)123456;
    score += (int32_t)(int64_t)-123456;
    score += (int32_t)(size_t)65432;
    score += (int32_t)(intptr_t)-65432;
    int32_t storage = 77;
    if (&storage != &storage) return 1;
    score += 1;
    int32_t token_storage = 19;
    void *token = &token_storage;
    if (token != &token_storage) return 2;
    score += 2;
    uint32_t vector[4] = {1, 2, 3, 4};
    score += (int32_t)(vector[0] + vector[1] + vector[2] + vector[3]);
    storage += 14;
    if (storage != 91) return 3;
    score += storage - 90;
    score += ordered;
    int8_t native_i8 = (int8_t)249;
    bool native_bool = true;
    if (native_i8 != (int8_t)249 || !native_bool) return 4;

    printf("%d %d %d %d %d\n", objects[0].total, objects[1].total, objects[2].total, calls, score);
    return 0;
}

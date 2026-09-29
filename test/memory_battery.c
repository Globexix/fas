#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

struct Resources {
    void *pointer;
    void *token;
};

extern uint32_t fas_order_array_assignment(void);
extern uint32_t fas_order_array_compound(void);
extern void fas_order_raw_store(void);
extern void fas_order_view_binding(void);
extern void fas_order_copy(void);
extern uint32_t fas_order_vector_lane(void);
extern uint32_t fas_raw_alias(void);
extern uint32_t fas_view_alias(void);
extern uint32_t fas_compound_alias(void);
extern uint32_t fas_vector_alias(void);
extern uint32_t fas_empty_views(void);
extern uint32_t fas_uninitialized_view(void);
extern bool fas_handle_round_trip(void *value);
extern void fas_copy_resources(void *destination, void *source);
extern uint32_t fas_foreign_effect(void);

static int events[64];
static size_t event_count;
static uint32_t raw_target[2];
static uint32_t copy_destination[2];
static uint32_t copy_source[2];
static uint32_t *held_value;
static uint32_t *foreign_value;
static uint32_t *held_vector;
static int compound_rhs_calls;
static int vector_rhs_calls;

static void record_event(int value)
{
    if (event_count < sizeof(events) / sizeof(events[0])) events[event_count++] = value;
}

void *battery_base(uint32_t tag)
{
    record_event((int)tag);
    if (tag == 12u) return copy_destination;
    if (tag == 14u) return copy_source;
    return raw_target;
}

size_t battery_index(uint32_t tag)
{
    record_event((int)tag);
    if (tag == 13u || tag == 15u) return 0u;
    return 1u;
}

uint32_t battery_rhs(uint32_t tag)
{
    record_event((int)tag);
    return tag * 10u;
}

void battery_hold(void *pointer)
{
    held_value = (uint32_t *)pointer;
}

uint32_t battery_compound_rhs(void)
{
    ++compound_rhs_calls;
    *held_value = 100u;
    return 3u;
}

void battery_hold_vector(void *pointer)
{
    held_vector = (uint32_t *)pointer;
}

uint32_t battery_vector_rhs(void)
{
    ++vector_rhs_calls;
    held_vector[1] = 90u;
    held_vector[2] = 91u;
    return 77u;
}

void battery_foreign_hold(void *pointer)
{
    foreign_value = (uint32_t *)pointer;
}

void battery_unrelated_write(void)
{
    *foreign_value = 143u;
}

static int check_events(const char *name, const int *expected, size_t count)
{
    if (event_count == count && memcmp(events, expected, count * sizeof(expected[0])) == 0) return 0;
    fprintf(stderr, "memory battery: %s event order/count mismatch\n", name);
    return 1;
}

static int check_value(const char *name, uint32_t got, uint32_t expected)
{
    if (got == expected) return 0;
    fprintf(stderr, "memory battery: %s got %u expected %u\n", name, got, expected);
    return 1;
}

static void reset_events(void)
{
    event_count = 0u;
}

int main(void)
{
    int failed = 0;
    const int assignment_events[] = {1, 2};
    const int compound_events[] = {4, 5};
    const int raw_events[] = {6, 7, 8};
    const int view_events[] = {9, 10, 11};
    const int copy_events[] = {12, 13, 14, 15};
    const int vector_events[] = {16, 17};
    uint32_t token_value = 0u;
    struct Resources source = {&token_value, &token_value};
    struct Resources destination = {NULL, NULL};

    reset_events();
    failed |= check_value("array assignment", fas_order_array_assignment(), 20u);
    failed |= check_events("array assignment", assignment_events, 2u);

    reset_events();
    failed |= check_value("array compound", fas_order_array_compound(), 57u);
    failed |= check_events("array compound", compound_events, 2u);

    memset(raw_target, 0, sizeof(raw_target));
    reset_events();
    fas_order_raw_store();
    failed |= check_events("raw place store", raw_events, 3u);
    failed |= check_value("raw place result", raw_target[1], 80u);

    memset(raw_target, 0, sizeof(raw_target));
    reset_events();
    fas_order_view_binding();
    failed |= check_events("view binding", view_events, 3u);
    failed |= check_value("view result", raw_target[1], 110u);

    copy_source[0] = 31u;
    copy_source[1] = 37u;
    copy_destination[0] = 0u;
    copy_destination[1] = 0u;
    reset_events();
    fas_order_copy();
    failed |= check_events("copy", copy_events, 4u);
    failed |= check_value("copy first element", copy_destination[0], 31u);
    failed |= check_value("copy second element", copy_destination[1], 37u);

    reset_events();
    failed |= check_value("vector lane", fas_order_vector_lane(), 170u);
    failed |= check_events("vector lane", vector_events, 2u);

    failed |= check_value("raw alias", fas_raw_alias(), 5u);
    failed |= check_value("view alias", fas_view_alias(), 7u);
    compound_rhs_calls = 0;
    failed |= check_value("compound old value", fas_compound_alias(), 13u);
    failed |= check_value("compound RHS count", (uint32_t)compound_rhs_calls, 1u);
    vector_rhs_calls = 0;
    failed |= check_value("vector alias lanes", fas_vector_alias(), 77995u);
    failed |= check_value("vector RHS count", (uint32_t)vector_rhs_calls, 1u);

    failed |= check_value("empty views", fas_empty_views(), 1u);
    failed |= check_value("uninitialized view write", fas_uninitialized_view(), 61u);
    failed |= check_value("handle round trip", (uint32_t)fas_handle_round_trip(&token_value), 1u);

    fas_copy_resources(&destination, &source);
    if (destination.pointer != source.pointer || destination.token != source.token) {
        fputs("memory battery: shallow resource copy failed\n", stderr);
        failed = 1;
    }

    foreign_value = NULL;
    failed |= check_value("foreign write after unrelated call", fas_foreign_effect(), 143u);

    if (failed) return 1;
    puts("memory battery: evaluation, aliasing, views, handles and foreign effects: ok");
    return 0;
}

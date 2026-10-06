static int yes_calls;
static int no_calls;

void reset_counts(void) {
    yes_calls = 0;
    no_calls = 0;
}

int yes_count(void) {
    return yes_calls;
}

int no_count(void) {
    return no_calls;
}

int yes_value(void) {
    ++yes_calls;
    return 37;
}

int no_value(void) {
    ++no_calls;
    return 41;
}

int oracle_value(int condition) {
    return condition ? 37 : 41;
}

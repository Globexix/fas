#define FAS_FEATURE 1
#include "feature.h"

int main(void) { return configured_value() == 41 ? 0 : 1; }

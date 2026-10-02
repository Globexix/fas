#include <stdint.h>

typedef struct FeatureRecord {
  int32_t value;
} FeatureRecord;

#ifdef FAS_FEATURE
static inline int configured_value(void) { return 41; }
#else
static inline int configured_value(void) { return 9; }
#endif

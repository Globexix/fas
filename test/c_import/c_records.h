#include <stdint.h>
#include <stdio.h>

typedef struct FasRecord {
  uint8_t kind;
  int32_t value;
  int32_t lanes[2];
} FasRecord;

typedef struct FasAddressEntry {
  char *name;
  int32_t *target;
  int32_t value;
} FasAddressEntry;

typedef struct FasLinkRecord {
  int32_t value;
  struct FasLinkRecord *link;
} FasLinkRecord;

#ifdef __cplusplus
extern "C" {
#endif

extern FasRecord fas_exported_records[2];
extern FasLinkRecord fas_link_table[3];
extern int32_t fas_completed_values[];
extern int32_t fas_constant_target;

void c_fill_record(FasRecord *record);
int32_t c_check_record(const FasRecord *record);
int32_t c_check_table(const FasAddressEntry *entries);
int32_t c_check_link_table(void);
int32_t c_check_and_mutate_exports(void);
void fas_write_record(FasRecord *record);
int32_t fas_read_c_filled(void);
int32_t fas_pass_local_to_c(void);
int32_t fas_check_table(void);
int32_t fas_read_exported_records(void);
int32_t fas_read_completed_values(void);

#ifdef __cplusplus
}
#endif

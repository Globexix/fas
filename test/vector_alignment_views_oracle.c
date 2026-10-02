#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

typedef uint32_t Vec4u32 __attribute__((ext_vector_type(4)));
struct RawVectorRecord {
  uint8_t prefix;
  Vec4u32 values;
};

extern uint32_t raw_vector_read(void *);
extern uint32_t raw_vector_write(void *);
extern uint32_t record_view_read(void *);
extern uint32_t record_field_view_write(void *);

int main(void) {
  _Alignas(16) unsigned char storage[64] = {0};
  uint32_t values[4] = {11, 22, 33, 44};
  memcpy(storage + 1, values, sizeof(values));
  uint32_t vector_sum = values[0] + values[1] + values[2] + values[3];
  uint32_t read_sum = raw_vector_read(storage + 1);
  uint32_t written_lane = raw_vector_write(storage + 1);
  uint32_t observed_lane = 0;
  memcpy(&observed_lane, storage + 1 + 2 * sizeof(uint32_t), sizeof(observed_lane));

  uint32_t record_values[4] = {5, 6, 7, 8};
  size_t field_offset = offsetof(struct RawVectorRecord, values);
  memcpy(storage + 1 + field_offset, record_values, sizeof(record_values));
  uint32_t record_sum = record_values[0] + record_values[1] + record_values[2] + record_values[3];
  uint32_t record_read = record_view_read(storage + 1);
  uint32_t field_written = record_field_view_write(storage + 1);
  uint32_t observed_field = 0;
  memcpy(&observed_field, storage + 1 + field_offset + sizeof(uint32_t), sizeof(observed_field));

  int ok = read_sum == vector_sum && written_lane == 77 && observed_lane == 77 &&
    record_read == record_sum && field_written == 88 && observed_field == 88;
  printf("vector=%u/%u lane=%u/%u record=%u/%u field=%u/%u\n",
    vector_sum, read_sum, 77u, observed_lane, record_sum, record_read, 88u, observed_field);
  return ok ? 0 : 1;
}

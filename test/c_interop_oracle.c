#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include "c_interop_address.h"

static void at_exit_output(void) { puts("exit"); }

static int compare_i32(const void *left, const void *right) {
  int a;
  int b;
  memcpy(&a, left, sizeof(a));
  memcpy(&b, right, sizeof(b));
  return (a > b) - (a < b);
}

static int call_int(int (*function)(int), int value) {
  return function(value);
}

int main(void) {
  int values[5] = {9, 1, 7, 2, 4};
  const char *path = "interop.bin";
  const char payload[] = "12345678";
  size_t size = strlen("fas");
  void *memory = malloc(size);
  struct stat stat_result;
  struct timespec clock_result;
  FILE *file;
  long end_position;
  int eof;
  int (*const functions[2])(int) = {fas_interop_static_helper,
                                    fas_interop_address_only};

  if (fas_interop_static_helper(1) != 2 ||
      call_int(fas_interop_static_helper, 41) != 42 ||
      call_int(functions[0], 8) != 9 || call_int(functions[1], 9) != 18)
    return 13;

  if (memory == NULL) return 1;
  free(memory);
  file = fopen(path, "wb");
  if (file == NULL) return 2;
  if (fwrite(payload, 1, strlen(payload), file) != 8) return 3;
  if (fclose(file) != 0) return 4;
  if (stat(path, &stat_result) != 0 || stat_result.st_size != 8) return 5;
  if (clock_gettime(CLOCK_MONOTONIC, &clock_result) != 0 ||
      clock_result.tv_sec < 0)
    return 6;
  file = fopen(path, "rb");
  if (file == NULL) return 7;
  if (fseek(file, 0, SEEK_END) != 0) return 8;
  end_position = ftell(file);
  if (end_position != 8) return 9;
  eof = getc(file);
  if (eof != EOF || fclose(file) != 0) return 10;
  qsort(values, 5, sizeof(values[0]), compare_i32);
  if (values[0] != 1 || values[1] != 2 || values[2] != 4 || values[3] != 7 ||
      values[4] != 9)
    return 11;
  if (atexit(at_exit_output) != 0) return 12;
  puts("interop: ok");
  puts("main");
  return 0;
}

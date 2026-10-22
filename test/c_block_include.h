#ifndef C_BLOCK_INCLUDE_H
#define C_BLOCK_INCLUDE_H
int cbi_count(const char *text, char wanted);
#endif
#ifdef C_BLOCK_INCLUDE_IMPLEMENTATION
int cbi_count(const char *text, char wanted) {
  int count = 0;
  for (; *text; text++) count += *text == wanted;
  return count;
}
#endif

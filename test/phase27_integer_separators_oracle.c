int main(void) {
  unsigned long long decimal = 1000ULL;
  unsigned long long hexadecimal = 16777619ULL;
  unsigned long long binary = 160ULL;
  return decimal == 1000ULL && hexadecimal == 16777619ULL && binary == 160ULL ? 0 : 1;
}

/* Copyright (c) Galois, Inc. 2024 */

#include <stdlib.h>
#include <time.h>

int test(int arg) {
  int res;
  res = res + arg;
  return res;
}

int main() {
  srand(time(NULL));
  int r = rand();
  return test(r);
}

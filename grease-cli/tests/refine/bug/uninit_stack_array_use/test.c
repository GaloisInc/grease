/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <stdlib.h>
#include <time.h>

int test(int arg) {
  int res[10];
  int rval;

  res[3] = arg * 2;
  res[5] = arg + res[3];
  rval = res[0] + res[5];

  return rval;
}

int main() {
  srand(time(NULL));
  int r = rand();
  return test(r);
}
// all: uninit_stack()

/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x270:malloc", "--plt-stub", "0x280:free"}
// all: go(prog)

#include <stdlib.h>

int test(int *x) {
  int* y = malloc(sizeof(int));
  free(y);
  return 0;
}
// all: ok()

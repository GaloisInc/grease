/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test
// flags(ppc32): --plt-stub 0x270:malloc --plt-stub 0x280:free

#include <stdlib.h>

int test(int *x) {
  int* y = malloc(sizeof(int));
  free(y);
  return 0;
}
// all: ok()

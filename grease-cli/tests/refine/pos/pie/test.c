/* Copyright (c) Galois, Inc. 2024 */

#include <stdlib.h>

int test(int *x) {
  int* y = malloc(sizeof(int));
  free(y);
  return 0;
}
// all: ok()

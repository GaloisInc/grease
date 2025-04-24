/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#156.
#include <stddef.h>

void *malloc(size_t size) {
  return 0;
}
void free(void *ptr) {}

void test() {
  int* x = malloc(sizeof(x));
  free(x);
  free(x);
}
/// must_fail()

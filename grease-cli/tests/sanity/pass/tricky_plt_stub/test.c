/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#179. We reference the `malloc` function by way
// of a function pointer so that there will be a PLT stub for `malloc` in the
// `.plt.got` section (at least, on x86-64).
#include <stdlib.h>

extern int a1, a2;

int b(void) {
  void* (*m)(size_t) = &malloc;
  int* x = malloc(sizeof(int));
  *x = 42;

  return a1 + a2;
}

int test(void) {
  return b();
}

// all: ok()

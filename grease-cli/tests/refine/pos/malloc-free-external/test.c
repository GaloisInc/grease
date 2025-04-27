/* Copyright (c) Galois, Inc. 2024 */

#include <stddef.h>
#include <stdlib.h>

// A variant of the malloc-free test case that dynamically links against libc
// instead of redefining malloc and free. This ensures that the calls to malloc
// and free come from an external shared library, which requires a different
// code path for function overrides that overriding functions defined in the
// same binary.

// flag(ppc32): --plt-stub 0x10000260:malloc --plt-stub 0x10000270:free

int test(void) {
  int* p = malloc(sizeof(int));
  *p = 42;
  int x = *p;
  free(p);
  return x;
}
// all: ok()

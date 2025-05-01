/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

__attribute__((noinline)) int dereferences_argument_16(int *ptr) {
  return ptr[16];
}
int test() {
  int x[8];
  return dereferences_argument_16(x);
}
// all: uninit_stack()

/* Copyright (c) Galois, Inc. 2024 */

// gitlab#47
unsigned int test(unsigned int x) {
  if (x == 0) {
    return 1;
  }
  return x * test(x - 1);
}
// all: could_not_infer()

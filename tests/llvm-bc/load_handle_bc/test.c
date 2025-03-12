/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#158.
void* f(void);

void* test(void) {
  return f();
}

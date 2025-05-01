/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#232. Because grease initializes empty
// PtrTargets to symbolic bitvectors, not symbolic pointers, the use of
// addition below (which macaw-symbolic interprets as a PtrAdd statement)
// should not fail.

// flags: --symbol test

unsigned int test(unsigned int a, unsigned int b) {
  return a + b;
}
// all: ok()

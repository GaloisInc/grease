/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

// Set --iters low to keep this test fast
// flags: --iters 8

int test(int *x) {
  return x[4096];
}
// TODO(#3)
// all: check "maximum iterations exceeded"

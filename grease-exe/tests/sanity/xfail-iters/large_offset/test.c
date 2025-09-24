/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}

// Set --iters low to keep this test fast
// all: flags {"--iters", "8"}
// all: go(prog)

int test(int *x) {
  return x[4096];
}
// TODO(#3)
// all: check "maximum iterations exceeded"

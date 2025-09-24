/* Copyright (c) Galois, Inc. 2025 */

// A regression test for #187, which was caused by the Macaw lazy memory model
// initializing the global memory on a per-path basis.

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <stdlib.h>

int counter = 1;

void test(int x) {
  if (x == 42) {
    counter = 0;
  }
  if (counter == 2) {
    abort();
  }
}

// all: ok()

/* Copyright (c) Galois, Inc. 2025 */

// A regression test for #187, which was caused by the Macaw lazy memory model
// initializing the global memory on a per-path basis.

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

// all: flags {"--symbol", "test"}
// all: go(prog)
// all: ok()

// These are could_not_infer because this was compiled with -nostdlib, so the
// call fails.
//
// arm: flags {"--globals", "symbolic"}
// arm: flags {"--symbol", "test"}
// arm: go(prog)
// arm: could_not_infer()
// arm: check "Failed to call function"
// x64: flags {"--globals", "symbolic"}
// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: could_not_infer()
// x64: check "Failed to call function"

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

// arm: ok()
// x64: ok()

// On PPC32, the memory model produces an ITE over a function-backed array for
// the global memory state after the first branch. Yices cannot handle this
// expression type, so the goal is reported as a possible bug rather than being
// proved.
//
// ppc32: check "we do not support if/then/else expressions at type function-backed array with solver Yices."
// ppc32: could_not_infer()

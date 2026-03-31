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
// arm: ok()
// x64: ok()

// On PPC32, the memory model produces an ITE over a function-backed array for
// the global memory state after the first branch. Yices cannot handle this
// expression type, so the goal is reported as a possible bug rather than being
// proved. This would work with another solver (e.g., it works with Z3).
//
// ppc32: check "we do not support if/then/else expressions at type function-backed array with solver Yices."
// ppc32: could_not_infer()

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

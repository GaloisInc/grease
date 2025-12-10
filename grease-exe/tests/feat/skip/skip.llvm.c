/* Copyright (c) Galois, Inc. 2025 */

// CFLAGS: $LLVM

// Test --skip for LLVM bitcode.

#include <stdlib.h>

extern void bug(void);

void do_abort(void) {
  abort();
}

/// flags {"--symbol", "test"}
/// go(prog)
/// must_fail()

/// flags {"--skip", "abort"}
/// flags {"--skip", "bug"}
/// flags {"--skip", "do_abort"}
/// flags {"--overrides", "tests/feat/skip/bug.aux.llvm.cbl"}
/// flags {"--symbol", "test"}
/// go(prog)
// LLVM inserts an `unreachable` instruction after the call to `abort`.
/// check "LLVM unreachable code"

void test(void) {
  bug();
  do_abort();
  abort();
}

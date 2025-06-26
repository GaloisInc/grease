/* Copyright (c) Galois, Inc. 2024 */

// Regression test for gitlab#170, test that "polymorphic" LLVM overrides such
// as `memset` are properly handled; and for gitlab#186, test that the
// heuristics handle `memset` by allocating enough space.

// CFLAGS: $LLVM

/// flags {"--symbol", "test"}
/// go(prog)

#include <string.h>

void *test(void *dest) {
  return memset(dest, 0, 16);
}
/// ok()

/* Copyright (c) Galois, Inc. 2025 */

// Test that built-in overrides are properly bound to their respective function
// allocations.
//
// Regression test for #508.

// CFLAGS: $LLVM

/// flags {"--symbol", "test"}
/// go(prog)

#include <stdlib.h>

void *(*g)(size_t) = malloc;

void test() {
  void *(*f)(size_t) = g;
  f(0);
}

/// check_not "Missing implementation for 'malloc'"
/// ok()

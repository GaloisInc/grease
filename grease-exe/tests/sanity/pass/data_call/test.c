/* Copyright (c) Galois, Inc. 2024 */

// Test that GREASE gracefully skips calls to functions placed in non-executable
// sections (e.g., .data).

// all: flags {"--symbol", "test"}
// all: go(prog)

__attribute__((section(".data"))) int inc(int x) { return x + 1; }

int test(int x) { return inc(x); }
// all: check "Skipped call to a non-executable address"
// all: ok()

/* Copyright (c) Galois, Inc. 2024 */

// Test that GREASE gracefully skips calls to functions placed in non-executable
// sections (e.g., .data), even when only some paths involve such calls.

// all: flags {"--symbol", "test"}
// all: go(prog)

__attribute__((section(".data"))) int inc(int x) { return x + 1; }

__attribute__((noinline)) int dec(int x) { return x - 1; }

int test(int x) {
  if (x) {
    return inc(x);
  }
  return dec(x);
}
// all: check "Skipped call to a non-executable address"
// all: ok()

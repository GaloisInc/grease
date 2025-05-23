/* Copyright (c) Galois, Inc. 2024 */

// This is undefined behavior at the source level, but is fine in a binary

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <stddef.h>

__attribute__((noinline)) long compare(long x, long y) { return x < y; }
long test() {
  long x;
  long y;
  return compare((long)&x, (long)&y);
}
// all: ok()

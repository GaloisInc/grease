/* Copyright (c) Galois, Inc. 2025 */

// Tests that we log a failure to parse for an unsupported dwarf type (Unions)
// See issue #262

typedef union y {
  long x;
  char z;
} y;

struct bar {
  int x;
  y *blah;
};

int foo(int x, int *y, struct bar *z) { return x + *y + z->x; }

// all: flags {"--symbol", "foo", "--debug-info-types", "precise-debug-info-shapes", "--no-heuristics"}
// x64: go(prog)
// x64: check("Failed to parse DWARF shape in")

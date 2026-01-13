/* Copyright (c) Galois, Inc. 2025 */

// Tests that with a conservative shape we a. dont try to parse a union
// b. dont attempt to make separate fields for bar

typedef union y {
  long x;
  char z;
} y;

struct bar {
  int x;
  y *blah;
};

int foo(int x, int *y, struct bar *z) { return x + *y + z->x; }

// all: flags {"--symbol", "foo", "--debug-info-types", "conservative-debug-info-shapes", "--no-heuristics"}
// x64: go(prog)

// Shows that we dont unfold the shape because we dont have a. a separate type
// for the fields of bar b. we should currently fail on the union

// x64: check_not("Failed to parse DWARF shape in")
// x64: check("XX*10")
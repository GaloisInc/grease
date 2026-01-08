/* Copyright (c) Galois, Inc. 2025 */

// Tests that we can still succeed without heuristics to load fields prior to a
// failure and after a function pointer

typedef union y {
  long x;
  char z;
} y;

struct bar {
  int (*somefn)(int);
  int x;
  y *blah;
};

int foo(int x, int *y, struct bar *z) { return x + *y + z->x; }

// all: flags {"--symbol", "foo", "--use-debug-info-types", "--no-heuristics"}
// x64: go(prog)
// x64: check("Failed to parse DWARF shape in")
// x64: ok()
/* Copyright (c) Galois, Inc. 2025 */

// Tests that we can parse an array via dwarf.

struct bar {
  int x;
  int arr[100];
};

int foo(int x, int *y, struct bar *z) { return x + *y + z->x + z->arr[50]; }

// all: flags {"--symbol", "foo", "--use-debug-info-types", "--no-heuristics"}
// x64: go(prog)
// x64: check("Failed to parse dwarf shape in")

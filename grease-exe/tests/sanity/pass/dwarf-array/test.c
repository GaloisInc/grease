/* Copyright (c) Galois, Inc. 2025 */

// Tests that we can parse an array via dwarf.

struct bar {
  int x;
  int arr[100];
};

int foo(int x, int *y, struct bar *z) { return x + *y + z->x + z->arr[50]; }

// all: flags {"--symbol", "foo", "--debug-info-types", "--precise-debug-info-shapes", "--no-heuristics"}
// x64: go(prog)
// x64: ok()

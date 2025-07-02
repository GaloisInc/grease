/* Copyright (c) Galois, Inc. 2025 */

// Tests that heuristics are unneeded when using dwarf populated shapes to 
// create a memory precondition that allows foo to execute.

struct bar {
  int x;
};

int foo(int x, int *y, struct bar *z) { return x + *y + z->x; }

// all: flags {"--symbol", "foo", "--use-debug-info-types", "--no-heuristics"}
// x64: go(prog)
// x64: ok()

/* Copyright (c) Galois, Inc. 2025 */

// Tests that heuristics are unneeded when using dwarf populated shapes to
// create a memory precondition that allows foo to execute. Uses a typedef
// and restrict, shows that GREASE parses through these DWARF tags.

typedef struct bar {
  int x;
} bar;

int foo(int x, int *restrict y, bar *restrict z) { return x + *y + z->x; }

// all: flags {"--symbol", "foo", "--debug-info-types", "precise-debug-info-shapes", "--no-heuristics"}
// x64: go(prog)
// x64: ok()
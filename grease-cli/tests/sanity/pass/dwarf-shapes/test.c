struct bar {
  int x;
};

int foo(int x, int *y, struct bar *z) { return x + *y + z->x; }

// all: flags {"--symbol", "foo", "--enable-dwarf-preconditions", "--no-heuristics"}
// x64: go(prog)
// x64: ok()
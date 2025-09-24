/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

typedef struct point {
  int x;
  int y;
} point;

int test(point *pt) { return pt->x; }
// all: ok()

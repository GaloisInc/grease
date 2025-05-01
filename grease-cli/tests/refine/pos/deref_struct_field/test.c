/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

typedef struct point {
  int x;
  int y;
} point;

int test(point *pt) { return pt->x; }
// all: ok()

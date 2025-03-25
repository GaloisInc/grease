/* Copyright (c) Galois, Inc. 2024 */

typedef struct point {
  int x;
  int y;
} point;

int test(point *pt) { return pt->x; }

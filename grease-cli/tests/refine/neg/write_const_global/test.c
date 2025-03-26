/* Copyright (c) Galois, Inc. 2024 */

const int x = 4;
__attribute__((noinline)) void make_it_five(int *ptr) { *ptr = 5; }
int test(int y) {
  make_it_five(&x);
  return y + x;
}

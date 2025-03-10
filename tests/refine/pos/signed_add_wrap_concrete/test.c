/* Copyright (c) Galois, Inc. 2024 */

// UB at the source level; no problem at the binary level.
__attribute__((noinline)) int add_max_int_minus_one(int x) {
  return x + 2147483646; // 2^32 - 2
}
int test(int x) {
  if (x) {
    return add_max_int_minus_one(4);
  }
  return 0;
}

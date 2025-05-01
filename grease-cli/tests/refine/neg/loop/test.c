/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

// Test that there are some default loop bounds that prevent this from spinning
unsigned int test(unsigned int n) {
  unsigned int sum = 0;
  for (unsigned int i = 0; i < n; i++) {
    sum += i;
  }
  return sum;
}
// all: could_not_infer()

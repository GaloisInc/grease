/* Copyright (c) Galois, Inc. 2024 */

int test(int *x) {
  return x[4096];
}
// TODO(#3)
// all: check "maximum iterations exceeded"

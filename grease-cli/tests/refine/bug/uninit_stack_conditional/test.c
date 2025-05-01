/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

#include <stdio.h>
#include <stdlib.h>

int test(int a) {
  int rval;
  if (a) {
    rval = 42;
  }
  return rval; // BUG: rval may not have been initialized
}

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("Usage: %s <a> <b>", argv[0]);
    return 1;
  }

  return test(atoi(argv[1]));
}
// arm: uninit_stack()
// TODO(#47):
// ppc32: could_not_infer()
// x64: could_not_infer()

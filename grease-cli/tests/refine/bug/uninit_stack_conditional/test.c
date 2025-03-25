/* Copyright (c) Galois, Inc. 2024 */

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
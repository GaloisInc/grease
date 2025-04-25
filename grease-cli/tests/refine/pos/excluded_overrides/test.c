/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#226, which ensures that we do not convert LLVM
// overrides to Macaw overrides that do not work well with macaw-symbolic's
// lazy memory model.
#include <assert.h>
#include <stdio.h>

void my_assert(int expression) {
    assert(expression);
}

int test(void) {
  printf("Hello, World!\n");
  putchar('H');
  puts("Hello, World!\n");
  my_assert(1);
}
// all: ok()

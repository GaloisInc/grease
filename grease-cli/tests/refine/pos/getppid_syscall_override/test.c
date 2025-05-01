/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

#include <sys/syscall.h>
#include <unistd.h>

void test(void) {
  getppid();
}
// all: ok()

/* Copyright (c) Galois, Inc. 2024 */

#include <sys/syscall.h>
#include <unistd.h>

void test(void) {
  getppid();
}
// all: ok()

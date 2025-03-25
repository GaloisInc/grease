/* Copyright (c) Galois, Inc. 2024 */

#include <unistd.h>

// A simple test that does nothing but call alarm(), which performs a syscall.
// Currently, GREASE will treat all syscalls as no-ops.
void test(void) {
  alarm(0);
}

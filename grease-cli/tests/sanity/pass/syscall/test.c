/* Copyright (c) Galois, Inc. 2024 */

#include <unistd.h>

// A simple test that does nothing but call alarm(), which performs a syscall.
// Currently, GREASE will treat all syscalls as no-ops.
void test(void) {
  alarm(0);
}
// TODO: Why does ARM have an uninitialized stack read here?
// arm: uninit_stack()
// TODO: Why does PPC fail here?
// ppc32: check "UnsupportedInstruction Instruction STMW"
// x64: ok()

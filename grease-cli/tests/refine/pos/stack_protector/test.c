/* Copyright (c) Galois, Inc. 2024 */

// A test case for gitlab#132. This doesn't really do anything interesting on
// its own. Instead, the interesting part is compiling this program with
// -fstack-protector-all, which causes the generated assembly code to emit
// additional buffer overflow checks.
//
// These checks are usually quite exotic at the binary level. We fully support
// these checks in x86-64, but only partially in AArch32, where they are only
// supported in statically linked binaries. (As such, we are careful to compile
// this program with -static.) We do not support these checks in PowerPC at all
// at the moment.

int test(void) {
  return 0;
}

int main(void) {
  return test();
}
// all: ok()

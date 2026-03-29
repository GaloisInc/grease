/* Copyright (c) Galois, Inc. 2026 */

// Test the shadow stack feature

// all: flags {"--symbol", "main"}
// all: go(prog)

__attribute__((noinline)) void zero(char *buf, int len) {
  for (int i = 0; i < len; i++) {
    buf[i] = 0;
  }
}

// optimize("O1") prevents the compiler from saving a frame pointer register
// (e.g. r31 on PPC32) between buf and the saved return address.  Without it,
// the overflow corrupts the frame pointer, causing a memory fault in the
// epilogue before the function returns -- preempting the shadow-stack check.
__attribute__((noinline, optimize("O1"))) void caller(void) {
  char buf[8];
#if defined(__powerpc__)
  // PPC32: buf at r1+8, saved LR at r1+36. Need 32 bytes to reach past LR.
  zero(buf, 32);
#else
  // ARM: buf at sp, saved LR at sp+12.
  // X86: buf at rsp, ret addr at rsp+8.
  zero(buf, 24);
#endif
}

int main() {
  caller();
  return 0;
}
// all: stack_corruption()

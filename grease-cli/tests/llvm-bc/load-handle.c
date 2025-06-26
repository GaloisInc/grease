/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#158.

// CFLAGS: -emit-llvm -frecord-command-line

/// flags {"--symbol", "test"}
/// flags {"--overrides", "tests/llvm-bc/extra/f.cbl"}
/// go(prog)

void* f(void);

void* test(void) {
  return f();
}
/// ok()

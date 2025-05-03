/* Copyright (c) Galois, Inc. 2024 */

/// flags {"--symbol", "test"}
/// go(prog)

extern int skippable(void);

int test(void) {
/// check "Invoking the 'skippable' function"
  return skippable();
}
/// ok()

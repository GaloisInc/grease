/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#233. This defines an entrypoint function with a
// very large number of arguments, which should guarantee that the last
// argument is spilled to the stack on all currently supported architectures.
// In order for GREASE to be able to simulate this program without spurious
// uninitialized stack read errors, we need to use the --stack-argument-slots
// command-line option.

// all: flags {"--symbol", "test"}
// arm: flags {"--stack-argument-slots", "5"}
// ppc32: flags {"--stack-argument-slots", "1"}
// x64: flags {"--stack-argument-slots", "3"}
// all: go(prog)

unsigned int test(unsigned int a, unsigned int b, unsigned int c, unsigned int d, unsigned int e, unsigned int f, unsigned int g, unsigned int h, unsigned int i) {
  return i;
}
// all: ok()

/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#160.
//
// On x86-64, __builtin_trap() compiles to a `ud2` instruction, which Macaw does
// not support, causing `grease` to throw an exception and terminate simulation
// at that point. In Rust binaries, it's common for the `panic!` macro to emit
// an error just before a trap, and we'd still like to report that error to the
// user. As such, we want to ensure that the call to `abort` in this program
// occurs without being superseded by the `ud2` instruction.

// all: flags {"--symbol", "test"}
// all: go(prog)

void abort(void) {}

void panic(void) {
    abort();
}

int test(void) {
    panic();
    __builtin_trap();
    return 0;
}
// all: must_fail()
// all: check "Call to abort"

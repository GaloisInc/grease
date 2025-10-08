/* Copyright (c) Galois, Inc. 2025 */

// Simple load test that is compiled and loaded as a non-elf

int test(int *x) { return *x; }

// arm: flags {"--load-base", "0x1000", "--address", "0x1000", "--raw-binary"}
// arm: go(prog)
// arm: ok()

// arm: flags {"--load-base", "0x1000", "--symbol", "test", "--raw-binary"}
// arm: go(prog)
// arm: check("An entrypoint was provided that was not an address")

// arm: flags {"--load-base", "0x1000", "--address", "0x800000", "--raw-binary"}
// arm: go(prog)
// arm: check("A provided address entrypoint could not be resolved to an address in memory: 0x800000")
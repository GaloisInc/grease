/* Copyright (c) Galois, Inc. 2026 */

// Tests that GREASE can load an ECFS snapshot and follow a PLT call
// from the executable into a shared library function.

// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: check "Calling a PLT stub (add_numbers)"
// x64: ok()

#include <unistd.h>

extern int add_numbers(int a, int b);

int test(int x, int y) {
    return add_numbers(x, y);
}

int main() {
    test(5, 3);
    sleep(30);  // Keep alive for snapshot
    return 0;
}

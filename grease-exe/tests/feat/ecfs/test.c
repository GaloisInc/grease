/* Copyright (c) Galois, Inc. 2026 */

// Tests that GREASE detects ECFS (process snapshot) files and reports
// that support is not yet implemented.
// TODO(#562): Update this test when ECFS support is implemented.

// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: check "ECFS (process snapshot) files are not yet supported"

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

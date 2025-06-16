/* Copyright (c) Galois, Inc. 2025 */

// all: flags {"--symbol", "test"}
// all: go(prog)

int test() {
    long x = 0xDEADBEEF;
    int(*fptr)()=(int(*)()) x;
    return fptr() + 1;
}

// all: check "Invalid address: 0xdeadbeef"

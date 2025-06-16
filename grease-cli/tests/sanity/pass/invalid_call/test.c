/* Copyright (c) Galois, Inc. 2025 */

// all: flags {"--symbol", "test"}
// all: go(prog)

int test() {
    long x = 0xDEADBEEF;
    int(*fptr)()=(int(*)()) x;
    return fptr() + 1;
}


// arm: check "Invalid address: 0xdeadbeef"
// on ppc the last two bits are cleared
// ppc32: check "Invalid address: 0xdeadbeec"
// x64: check "Invalid address: 0xdeadbeef"
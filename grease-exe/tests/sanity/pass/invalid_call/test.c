/* Copyright (c) Galois, Inc. 2025 */


int test() {
    long x = 0xDEADBEEF;
    int(*fptr)()=(int(*)()) x;
    return fptr() + 1;
}

// all: flags {"--symbol", "test"}
// all: go(prog)
// arm: check "Invalid address: 0xdeadbeef"
// on ppc the last two bits are cleared
// ppc32: check "Invalid address: 0xdeadbeec"
// x64: check "Invalid address: 0xdeadbeef"

// all: flags {"--symbol", "test", "--skip-invalid-call-addrs"}
// all: go(prog)
// arm: check "TopV where PSTATE_T expected"
// x64: ok()
// ppc32: ok()
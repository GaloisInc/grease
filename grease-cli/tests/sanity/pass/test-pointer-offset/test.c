/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test", "--initial-precondition",  "init.shape"}
// x64: go(prog)

struct foo {
    char* begin;
    char* end;
};



int test(struct foo* in) {
    return *(in->end-1) - *in->begin;
}


// x64: ok()

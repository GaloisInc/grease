/* Copyright (c) Galois, Inc. 2024 */

// x64: flags {"--symbol", "test", "--no-heuristics", "--initial-precondition", "tests/sanity/pass/test-pointer-offset/init.shape"}
// x64: go(prog)

struct foo {
    char* begin;
    char* end;
};



int test(struct foo* in) {
    return *(in->end-1) - *in->begin;
}


// x64: ok()

/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

int *glob = 0;
int test() { return *glob; }
// all: could_not_infer()

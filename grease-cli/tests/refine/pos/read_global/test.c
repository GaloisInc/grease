/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

int glob = 4;
int test() { return glob; }
// all: ok()

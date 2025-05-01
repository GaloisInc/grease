/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

int glob = 4;
int test() { return glob; }
// all: ok()

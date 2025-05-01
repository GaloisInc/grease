/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

int *glob = 0;
int test() { return *glob; }
// all: could_not_infer()

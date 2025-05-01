/* Copyright (c) Galois, Inc. 2024 */

// grease does not (yet) execute functions marked `__attribute__
// ((constructor))` before analysis.

// flags: --symbol test

int g = 0;
int *g_ptr;

void __attribute__((constructor)) pre_main() { g_ptr = &g; }

int test(void) { return *g_ptr; }
// all: could_not_infer()

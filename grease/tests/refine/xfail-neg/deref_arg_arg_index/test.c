/* Copyright (c) Galois, Inc. 2024 */

// grease does not yet support this kind of precondition that depends on a
// relation between two arguments
int test(int *ptr, unsigned int idx) { return ptr[idx]; }

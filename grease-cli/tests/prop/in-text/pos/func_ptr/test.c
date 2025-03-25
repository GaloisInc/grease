/* Copyright (c) Galois, Inc. 2024 */

// The correct precondition for this function is complex... Not clear if it's
// worth it to bake this logic into grease.
int test(int (*fun_ptr)()) { return fun_ptr(); }
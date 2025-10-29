/* Copyright (c) Galois, Inc. 2025 */

// TODO: hack for now
// all: flags {"--symbol", "test_syscall"}
// arm: go_with_abort_syscalls(prog)
// arm: check_not "Skipped syscall"
// arm: check "abort sim for syscall"
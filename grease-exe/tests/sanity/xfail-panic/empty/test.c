/* Copyright (c) Galois, Inc. 2024 */

// This file does *not* have the expected `test` function.
// See issue gitlab#41.

// all: flags {"--symbol", "test"}
// all: go(prog)

// all: check [[Exception: Could not find entrypoint symbol "test"]]

#!/usr/bin/env bash

# Extract C compiler flags embedded in test files. See `doc/dev/tests.md` and
# the Makefile.

set -eu

grep '^// CFLAGS: ' "${1}" | sed 's|^// CFLAGS: ||'

#!/usr/bin/env python3
#
# Extract C compiler flags embedded in test files. See `doc/tests.md` and
# the Makefile.
#
# This script is largely copied from GREASE (see
# https://github.com/GaloisInc/grease/blob/5802e9b03d848611eeba0ce1fa49093db7e32f8e/grease-cli/tests/extract-cflags.py ),
# and any improvements in GREASE's script should be copied here as well.

from argparse import ArgumentParser
from pathlib import Path
from sys import exit, stderr


COMMENT = "// CFLAGS: "

GROUPS = {
    # The set of CC flags that we use across all test cases.
    #
    # * -fno-stack-protector: We want to check for this kind of error in
    #   screach.
    # * -nostartfiles: Make binaries smaller by not including the
    #   startup-related code that GCC links in by default. The trade-off is that
    #   the binaries GCC compiles are not fully formed due to their lack of a
    #   _start() function, but this is a small price to pay for the amount of
    #   code that we get rid of.
    "CFLAGS_COMMON": ["-fno-stack-protector", "-nostartfiles"],
    # A set of CC flags that makes binaries smaller by not linking against any
    # standard C libraries. This is reasonable for small test cases, but it is
    # not suitable for test cases that rely on functions from external libraries
    # (e.g., libc).
    "CFLAGS_NO_LIBS": ["-nodefaultlibs", "-nolibc", "-nostdlib"],
    # A set of CC flags that makes binaries more self-contained:
    #
    # * -static: Generally, we prefer self-contained test binaries.
    # * -no-pie: Position-independent executables are typically much larger.
    "CFLAGS_STATIC": ["-static", "-no-pie"],
}


def eprint(*args, **kwargs):
    print(*args, file=stderr, **kwargs)


def die(msg):
    eprint(msg)
    exit(1)


def gather(txt):
    raw_flags = []
    for line in txt.splitlines():
        if line.startswith(COMMENT):
            flags = line.removeprefix(COMMENT)
            raw_flags.append(flags)
    return raw_flags


def process(raw_flags):
    processed_flags = []
    for flag in raw_flags:
        if flag.startswith("$"):
            flag = flag.removeprefix("$")
            try:
                processed_flags += GROUPS[flag]
            except KeyError:
                die("unknown flag group: ${flag}")
        else:
            processed_flags.append(flag)
    return processed_flags


def extract(path):
    if not path.is_file():
        die("not a file: {path}")

    raw_flags = gather(path.read_text())
    flags = process(raw_flags)
    return " ".join(flags)


parser = ArgumentParser()
parser.add_argument("file", type=Path)
args = parser.parse_args()
print(extract(args.file))

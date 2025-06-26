#!/usr/bin/env python3
#
# Extract C compiler flags embedded in test files. See `doc/dev/tests.md` and
# the Makefile.

from argparse import ArgumentParser
from pathlib import Path
from sys import exit, stderr


COMMENT = "// CFLAGS: "

GROUPS = {"LLVM": ["-emit-llvm", "-frecord-command-line"]}


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

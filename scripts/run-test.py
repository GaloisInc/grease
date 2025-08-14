#!/usr/bin/env python3

# Run GREASE on a test with the flags specified by the test
#
# Only supports .cbl test cases at the moment.

from argparse import ArgumentParser
from pathlib import Path
from subprocess import run


def go(path: Path, /, *, dry_run: bool = False):
    txt = path.read_text()
    flags = []
    for line in txt.splitlines():
        if line.startswith(";; flags"):
            line = line.removeprefix(";; flags {")
            line = line.removesuffix("}")
            line_flags = line.split(",")
            for flag in line_flags:
                flag = flag.strip()
                flag = flag.removeprefix('"')
                flag = flag.removesuffix('"')
                flags.append(flag)
        elif line.startswith(";; go"):
            args = ["cabal", "run", "exe:grease", "--"] + flags + [str(path)]
            flags = []
            print(" ".join(args))
            if not dry_run:
                run(args, check=True)


parser = ArgumentParser()
parser.add_argument("path", type=Path)
parser.add_argument("-n", "--dry-run", action="store_true")
args = parser.parse_args()
go(args.path, dry_run=args.dry_run)

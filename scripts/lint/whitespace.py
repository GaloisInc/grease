#!/usr/bin/env python3

# Lint for end-of-line whitespace

from argparse import ArgumentParser
from pathlib import Path
from sys import exit


def die(msg):
    print(msg)
    exit(1)


def check(paths):
    for path in paths:
        if path.is_dir():
            files = path.rglob("*")
        else:
            files = [path]
        for file in files:
            if not file.is_file():
                continue
            try:
                t = file.read_text()
            except UnicodeDecodeError:
                continue
            for no, line in enumerate(t.splitlines()):
                if line != line.rstrip():
                    die(f"Trailing whitespace at {file}:{no + 1}")


parser = ArgumentParser()
parser.add_argument("paths", nargs="+", type=Path)
args = parser.parse_args()
check(args.paths)

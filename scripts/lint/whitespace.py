#!/usr/bin/env python3

"""Lint for end-of-line whitespace"""

from argparse import ArgumentParser
from pathlib import Path
from sys import exit


def die(msg, /):
    print(msg)
    exit(1)


def error(msg, /, *, fix: bool = False):
    if fix:
        print(msg)
    else:
        die(msg)


def check_file(path, /, *, fix: bool = False):
    if not path.is_file():
        return
    try:
        t = path.read_text()
    except UnicodeDecodeError:
        return
    fixed = []
    changed = False
    if not t.endswith("\n"):
        error(f"No trailing newline in {path}", fix=fix)
        changed = True
    if t.endswith("\n\n"):
        error(f"Multiple trailing newlines in {path}", fix=fix)
        changed = True
        t = t.rstrip()
    for no, line in enumerate(t.splitlines()):
        stripped = line.rstrip()
        fixed.append(stripped)
        if line != stripped:
            error(f"Trailing whitespace at {path}:{no + 1}", fix=fix)
            changed = True
    if changed and fix:
        path.write_text("\n".join(fixed) + "\n")


def check(paths, /, *, fix: bool = False):
    for path in paths:
        if path.is_dir():
            files = path.rglob("*")
        else:
            files = [path]
        for file in files:
            if not file.is_file():
                continue
            check_file(file, fix=fix)


parser = ArgumentParser(description=__doc__)
parser.add_argument("--fix", action="store_true")
parser.add_argument("paths", nargs="+", type=Path)
args = parser.parse_args()
check(args.paths, fix=args.fix)

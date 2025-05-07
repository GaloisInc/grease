#!/usr/bin/env python3

# Lint for end-of-line whitespace

from pathlib import Path
from sys import argv, exit


def die(msg):
    print(msg)
    exit(1)


for arg in argv[1:]:
    path = Path(arg)
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

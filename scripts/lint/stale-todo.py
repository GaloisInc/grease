#!/usr/bin/env python3

# Use the GitHub CLI to find stale TODOs of the form `TODO(#N)`

from functools import cache
from pathlib import Path
from re import compile
from subprocess import run
from sys import argv, exit

def die(msg):
    print(msg)
    exit(1)

@cache
def status(no):
    """Get the status (OPEN or CLOSED) of a GitHub issue using the `gh` CLI"""
    result = run(
        ['gh', 'issue', 'view', no, '--json', 'state', '--jq', '.state'],
        capture_output=True,
        text=True
    )
    out = result.stdout.strip()
    err = result.stderr.strip()
    if result.returncode != 0:
        die(f"Error retrieving issue {no}: {out} {err}")
    return out

r = compile(r"TODO\(#(\d+)\)")
for dir in argv[1:]:
    for file in Path(dir).rglob('*'):
        if not file.is_file():
            continue
        try:
            t = file.read_text()
        except UnicodeDecodeError:
            continue
        for m in r.finditer(t):
            no = m.group(1)
            s = status(no)
            print(f"{file} TODO(#{no}): {s}")
            if s != "OPEN":
                die(f"Stale TODO for {no}!")

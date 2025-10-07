#!/usr/bin/env python3

# Find modules that are frequently imported qualified, but have no corresponding
# hlint rule.

import re
from argparse import ArgumentParser
from collections import defaultdict
from pathlib import Path

RULE_PAT = re.compile(r"^- \{ name: ([A-Z][A-Za-z0-9_.]*), as: \w+ \}$")
IMPORT_PAT = re.compile(r"^import ([A-Z][A-Za-z0-9_.]*) qualified", flags=re.MULTILINE)


def parse_hlint_yaml(p: Path) -> set[str]:
    return {m.group(1) for m in RULE_PAT.finditer(p.read_text())}


def find_imports(p: Path) -> list[str]:
    return [m.group(1) for m in IMPORT_PAT.finditer(p.read_text())]


def go(top: int, /):
    root = Path(".")
    hlint_rules = parse_hlint_yaml(root / ".hlint.yaml")
    hs = root.rglob("grease*/**/*.hs")
    imports = defaultdict(int)

    for p in hs:
        for mod in find_imports(p):
            if mod not in hlint_rules:
                imports[mod] += 1

    sorted_modules = sorted(imports.items(), key=lambda x: x[1], reverse=True)
    mods = sorted_modules[: args.top]
    for i, (mod, count) in enumerate(mods, 1):
        print(f"{i:2d} {mod:<40} {count}")

    return 0


parser = ArgumentParser()
parser.add_argument("--top", type=int, default=10, help="Number of modules to show")
args = parser.parse_args()
go(args.top)

#!/usr/bin/env python3
"""Run grease benchmarks and collect GHC RTS performance metrics.

Each .c file in this directory is a self-contained benchmark. Two comment
directives near the top of each file control how it is built and run:

  // CFLAGS: <clang flags>   — how to compile the file to an ELF
  // GREASE: <grease args>   — arguments passed to grease after the ELF path
"""

import os
import re
import subprocess
import sys
from pathlib import Path

_BENCHMARKS_DIR = Path(__file__).resolve().parent
_PERF_PY = _BENCHMARKS_DIR.parent.parent / "scripts" / "perf.py"


def _parse_directive(c_file: Path, key: str) -> str | None:
    """Extract a // KEY: value directive from the top comment block of a C file."""
    pattern = re.compile(r"//\s*" + re.escape(key) + r":\s*(.*)")
    with open(c_file) as f:
        for line in f:
            line = line.strip()
            if not line.startswith("//"):
                break
            m = pattern.match(line)
            if m:
                return m.group(1).strip()
    return None


def _require_directive(c_file: Path, key: str) -> str:
    value = _parse_directive(c_file, key)
    if value is None:
        raise SystemExit(f"Error: {c_file.name} is missing // {key}: directive")
    return value


def _run_benchmark(c_file: Path, elf_path: str) -> None:
    grease_str = _require_directive(c_file, "GREASE")
    expected_output = _require_directive(c_file, "OUTPUT")

    result = subprocess.run(
        [
            sys.executable,
            str(_PERF_PY),
            "--package",
            "exe:grease",
            "--expect",
            expected_output,
            "--",
            elf_path,
        ]
        + grease_str.split(),
        capture_output=True,
        text=True,
    )
    print(result.stdout, end="")
    if result.returncode != 0:
        raise SystemExit(result.stderr or f"Error: {c_file.name}: perf.py failed")


def main() -> int:
    for c_file in sorted(_BENCHMARKS_DIR.glob("*.c")):
        cflags_str = _require_directive(c_file, "CFLAGS")

        elf_path = str(_BENCHMARKS_DIR / (c_file.stem + ".elf"))

        try:
            subprocess.check_call(
                ["clang", "-o", elf_path] + cflags_str.split() + [str(c_file)],
            )
            _run_benchmark(c_file, elf_path)
        finally:
            os.unlink(elf_path)

    return 0


if __name__ == "__main__":
    sys.exit(main())

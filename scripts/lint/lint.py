#!/usr/bin/env python

"""Run linters incrementally and in parallel using Ninja (see ./doc/dev.md)"""

# * Ninja
#
# Ninja is essentially a simpler, faster version of Make. A Ninja configuration
# consists of *rules* (`rule`) and some number of *build statements* (`build`).
# A rule is an abbreviation for a shell command, and a build statement is a
# recipe for producing some number of output files (*targets*) from some number
# of input files by running a rule.
#
# Conceptually, we can consider the Ninja configuration as a hypergraph where
# the nodes are files and the hyperedges are build statements, labeled by rules.
# The inputs to Ninja are this hypergraph and a set of desired targets. Ninja
# traverses the hypergraph and recursively runs rules to build everything until
# it can build the desired targets.
#
# Just like Make, Ninja checks *file modification times* to see if rebuilding
# is necessary. If the output was modified more recently than all of the inputs
# (according to filesystem metadata), then Ninja will skip rebuilding that
# target.
#
# See ninja.build for more information about Ninja.
#
# * This script
#
# This script generates Ninja configurations to run linters. It works by
# generating build statements that produce one target file (in `.out/`) for each
# combination of linter and input files. For example, for a file `foo/bar.py`
# and the linter `ruff`, it would generate a `build` statement like
#
#     build .out/foo-bar.py: ruff-check foo/bar.py
#
# where the `ruff-check` rule is something like:
#
#     rule ruff-check
#       command = ruff check -- $in && touch $out
#
# Together, these say "to produce output file `.out/foo-bar.py`, run `ruff
# check` on `foo/bar.py` and then (if it succeeds) `touch foo-bar.py`". Hence,
# the rules produce empty output files in `.out/` indicating that the linter has
# been run.

from argparse import ArgumentParser
from os import execvp
from pathlib import Path
from subprocess import run


ninja = """
builddir=.out/

# ---------------------------------------------------------
# haskell

rule hlint
  command = hlint -- $in && touch $out
  description = hlint

# ---------------------------------------------------------
# markdown

rule mdlynx
  command = mdlynx $in && touch $out
  description = mdlynx

rule typos
  command = typos $in && touch $out
  description = typos

# ---------------------------------------------------------
# python

rule ruff-check
  command = ruff check --quiet -- $in && touch $out
  description = ruff check

rule ruff-fmt
  command = ruff format --check --quiet -- $in && touch $out
  description = ruff format

# ---------------------------------------------------------
# scala

rule spotless
  command = cd ghidra-plugin && ./gradlew spotlessCheck && touch $out
  description = spotless

# ---------------------------------------------------------
# text

rule merge
  command = grep -E '^(<<<<<<<|=======|>>>>>>>)' -- $in && exit 1 || touch $out
  description = check for merge conflict markers

rule todo
  command = python3 scripts/lint/stale-todo.py -- $in && touch $out
  description = todo

rule ws
  command = python3 scripts/lint/whitespace.py -- $in && touch $out
  description = whitespace

"""


def build(outs: str, rule: str, ins: str):
    global ninja
    ninja += f"build {outs}: {rule} {ins}\n"


def lint(rule: str, ins: str):
    slug = ins.replace("/", "-") + "." + rule
    build(f"$builddir/{slug}", rule, ins)


def ls_files(pat: str) -> list[str]:
    out = run(
        ["git", "ls-files", "--exclude-standard", "--", pat],
        capture_output=True,
        shell=False,
    )
    return out.stdout.strip().decode("utf-8").split("\n")


def txt(path: str):
    lint("merge", path)
    lint("todo", path)
    lint("ws", path)


def hs():
    cb = ls_files("*.cabal")
    for path in cb:
        txt(path)

    hs = ls_files("*.hs")
    for path in hs:
        if "elf-edit-core-dump" in path:
            continue
        lint("hlint", path)
        txt(path)


def md():
    md = ls_files("*.md")
    for path in md:
        lint("mdlynx", path)
        lint("typos", path)
        txt(path)


def py():
    py = ls_files("*.py")
    for path in py:
        lint("ruff-check", path)
        lint("ruff-fmt", path)
        txt(path)


def scala():
    scala = ls_files("*.scala")
    build("$builddir/spotless", "spotless", " ".join(scala))
    for path in scala:
        txt(path)


def go(languages: list[str]):
    if languages is None or "hs" in languages:
        hs()
    if languages is None or "md" in languages:
        md()
    if languages is None or "py" in languages:
        py()
    if languages is None or "scala" in languages:
        scala()
    Path("build.ninja").write_text(ninja)
    execvp("ninja", ["ninja"])


parser = ArgumentParser(description=__doc__)
parser.add_argument("-l", "--language", action="append")
# ignore paths passed by ghcid --lint
parser.add_argument("IGNORED", nargs="*")
args = parser.parse_args()
go(args.language)

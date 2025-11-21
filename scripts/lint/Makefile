# This Makefile runs our linters. It works via pattern rules that produce
# one target file (in `.out/`) for each combination of linter and input file.
# For example, for a file `foo/bar.py` and the linter `ruff`, it essentially
# generates a rule like:
#
#     .out/foo/bar.py: foo/bar.py
#         ruff check foo/bar.py && mkdir -p .out/foo && touch .out/foo/bar.py
#
# Hence, the rules produce empty output files in `.out/` indicating that the
# linter has been run.
#
# Syntax primer:
#
# - `@` prevents Make from echoing the command, we explicitly `printf` instead
# - `$<` is the first prerequisite
# - `$@` is the output
# - `--` is the standard "end of options" marker for shell commands

LS_FILES := git ls-files --exclude-standard
OUT := .out
$(shell mkdir -p $(OUT))

SHELL = bash -euo pipefail

# all rules should depend on the Makefile itself
THIS = $(abspath $(lastword $(MAKEFILE_LIST)))

.PHONY: all
all:

.PHONY: clean
clean:
	rm -rf $(OUT)/*

.PHONY: help
help:
	@printf 'targets:\n'
	@grep '^\.PHONY: ' $(THIS) | cut -d ' ' -f2

# Used in most of the rules
touch = mkdir -p -- '$(dir $(1))' && touch -- '$(1)'

# ---------------------------------------------------------
# format

all: fmt
.PHONY: fmt
FMT_HS = $(shell $(LS_FILES) '*.hs')

fmt: fourmolu
.PHONY: fourmolu
fourmolu: $(addsuffix .fourmolu, $(addprefix $(OUT)/, $(FMT_HS)))
$(OUT)/%.hs.fourmolu: %.hs $(THIS)
	@printf "fourmolu %s\n" '$<'
	@fourmolu --mode=inplace --quiet -- '$<' && $(call touch,$@)

# ---------------------------------------------------------
# haskell

all: hs
.PHONY: hs
HS = $(shell $(LS_FILES) 'grease*/**/*.hs')

hs: hlint
.PHONY: hlint
hlint: $(addsuffix .hlint, $(addprefix $(OUT)/, $(HS)))
$(OUT)/%.hs.hlint: %.hs $(THIS)
	@printf "hlint %s\n" '$<'
	@hlint --quiet --hint=$(dir $(THIS))/../../.hlint.yaml -- '$<' && $(call touch,$@)

# ---------------------------------------------------------
# make

all: make
.PHONY: make
MAKES = $(shell $(LS_FILES) '**/Makefile' '*.mk')

# this checks make syntax, -n is a POSIX flag that means "don't run"
make: make-n
.PHONY: make-n
make-n: $(addsuffix .make-n, $(addprefix $(OUT)/, $(MAKES)))
$(OUT)/%.make-n: % $(THIS)
	@printf "make -n %s\n" '$<'
	@make -n -- '$<' && $(call touch,$@)

# ---------------------------------------------------------
# markdown

all: md
.PHONY: md
MD = $(shell $(LS_FILES) '*.md')

md: mdlynx
.PHONY: mdlynx
mdlynx: $(addsuffix .mdlynx, $(addprefix $(OUT)/, $(MD)))
$(OUT)/%.md.mdlynx: %.md $(THIS)
	@printf "mdlynx %s\n" '$<'
	@mdlynx -- '$<' && $(call touch,$@)

md: typos
.PHONY: typos
typos: $(addsuffix .typos, $(addprefix $(OUT)/, $(MD)))
$(OUT)/%.md.typos: %.md $(THIS)
	@printf "typos %s\n" '$<'
	@typos -- '$<' && $(call touch,$@)

# ---------------------------------------------------------
# python

all: py
.PHONY: py
PY = $(shell $(LS_FILES) '*.py')

py: ruff-check
.PHONY: ruff-check
ruff-check: $(addsuffix .ruff-check, $(addprefix $(OUT)/, $(PY)))
$(OUT)/%.py.ruff-check: %.py $(THIS)
	@printf "ruff check %s\n" '$<'
	@ruff check --quiet -- '$<' && $(call touch,$@)

py: ruff-format
.PHONY: ruff-format
ruff-format: $(addsuffix .ruff-format, $(addprefix $(OUT)/, $(PY)))
$(OUT)/%.py.ruff-format: %.py $(THIS)
	@printf "ruff format --check %s\n" '$<'
	@ruff format --check --diff --quiet -- '$<' && $(call touch,$@)

# ---------------------------------------------------------
# scala

all: scala
.PHONY: scala
SCALA = $(shell $(LS_FILES) '*.scala')

scala: spotless
.PHONY: spotless
spotless: $(addsuffix .spotless, $(addprefix $(OUT)/, $(SCALA)))
$(OUT)/spotless: $(SCALA) $(THIS)
	@printf "spotless\n"
	@cd ghidra-plugin && ./gradlew spotlessCheck && $(call touch,$@)

# ---------------------------------------------------------
# shell

all: sh
.PHONY: sh
SH = $(shell $(LS_FILES) '*.sh')

# this checks shell syntax, -n is a POSIX flag that means "don't run"
sh: sh-n
.PHONY: sh-n
sh-n: $(addsuffix .sh-n, $(addprefix $(OUT)/, $(SH)))
$(OUT)/%.sh.sh-n: %.sh $(THIS)
	@printf "sh -n %s\n" '$<'
	@sh -n -- '$<' && $(call touch,$@)

# ---------------------------------------------------------
# text

all: txt
.PHONY: txt
TXT = $(shell $(LS_FILES) '**/Makefile' '*.cabal' '*.md' '*.mk' '*.project' '*.py' '*.scala' '*.sh')

txt: merge
.PHONY: merge
merge: $(addsuffix .merge, $(addprefix $(OUT)/, $(TXT)))
$(OUT)/%.merge: % $(THIS)
	@printf "check for merge conflicts in %s\n" '$<'
	@grep -E '^(<<<<<<<|=======|>>>>>>>)' -- '$<' && exit 1 || $(call touch,$@)

# The other rules all run a single linter per input file. This is nice, because
# it lets Make run a bunch of processes in parallel. However, we only want
# to run a *single* instance of `stale-todo.py`, no matter how many files
# it's checking. This is because it makes network request to GitLab, but it
# internally caches the results of those requests. Minimizing network requests
# saves much more time than processing the files in parallel.
#
# We accomplish this by using a single output with *all* of the relevant files
# as input, in combination with the "automatic variable" `$?`. It expands to all
# of the inputs that are newer than the target.
txt: todo
.PHONY: todo
todo: $(OUT)/todo
$(OUT)/todo: $(TXT) scripts/lint/stale-todo.py $(THIS)
	@printf "check for outdated TODOs\n"
	@python3 scripts/lint/stale-todo.py -- $? && $(call touch,$@)

txt: whitespace
.PHONY: whitespace
whitespace: $(addsuffix .whitespace, $(addprefix $(OUT)/, $(TXT)))
$(OUT)/%.whitespace: % scripts/lint/whitespace.py $(THIS)
	@printf "check for trailing whitespace in %s\n" '$<'
	@python3 scripts/lint/whitespace.py -- '$<' && $(call touch,$@)

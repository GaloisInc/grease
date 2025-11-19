# Linting

## Makefile

GREASE employs a variety of linting tools, which are coordinated via Make. We
do our best to keep the `Makefile` up-to-date, but the ground truth of which
linters are run on what files and in what configuration is always supplied by
the CI system.

To list the targets:

```sh
make -f scripts/lint/Makefile help
```

To run them all:

```sh
make -j4 -f scripts/lint/Makefile
```

When using [ghcid], the linting script can be run on every save with:
```sh
ghcid \
  --command "cabal repl lib:grease pkg:grease-cli pkg:grease-exe" \
  --lint='make -j4 -f scripts/lint/Makefile hs'
```

[ghcid]: https://github.com/ndmitchell/ghcid

To create a pre-commit hook, simply add the `make` invocation to the Fourmolu
pre-commit hook described below.

## Generic scripts

We have a few Python scripts in `scripts/lint/` that perform one-off
checks. They generally take some number of paths as arguments, check
`.github/workflows/lint.yml` to see how they are invoked in CI.

## Fourmolu

This repo enforces Fourmolu formatting based on the committed configuration in `fourmolu.yaml`. Installation and usage directions are available
[here](https://github.com/fourmolu/fourmolu). In short:

```sh
cabal install fourmolu-0.19.0.0
fourmolu --mode inplace $(git ls-files '*.hs')
```

One can configure [auto-format on save](https://code.visualstudio.com/docs/editing/codebasics#_formatting) to avoid formatting issues.
The repo is already formatted with fourmolu so new formatting changes should be localized to behavioral changes. Further discussion of the rationale for the enforcement of a fourmolu style and commit hygiene is available in this [formatting discussion](./formatting.md).

To create a [pre-commit hook](https://git-scm.com/docs/githooks#_pre_commit):
```
cat <<'EOF' > .git/hooks/pre-commit
#!/usr/bin/env bash

# Run Fourmolu on changed files before committing

set -eu

files=$(git diff --diff-filter=d --name-only --cached -- '*.hs')
if [[ -n "${files}" ]]; then
    fourmolu --mode inplace ${files[@]}
    git add ${files[@]}
fi
EOF
chmod +x .git/hooks/pre-commit
```

## hlint

We treat a small number of hlint warnings as errors in CI. To run hlint locally, try:

```sh
hlint grease{,-aarch32,-ppc,-x86}/src grease-cli/src grease-exe/{main,src,tests}
```

## mdlynx

We run [mdlynx] on our Markdown files to check for broken links. To run it
locally, try:

```bash
git ls-files -z --exclude-standard '*.md' | xargs -0 mdlynx
```

[mdlynx]: https://github.com/langston-barrett/mdlynx

## ruff

We lint and format the Python linting scripts and Ghidra plug-in with [ruff].

```sh
git ls-files -z --exclude-standard '*.py' | xargs -0 ruff format
git ls-files -z --exclude-standard '*.py' | xargs -0 ruff check
```

[ruff]: https://docs.astral.sh/ruff/

## spotless

See the [Ghidra batch plugin docs](../ghidra-batch-plugin.md).

## typos

We run [typos] on `doc/` to spell-check the documentation. To run it locally,
try:

```bash
typos doc/
```

[typos]: https://github.com/crate-ci/typos

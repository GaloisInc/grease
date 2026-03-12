# Linting

## Lūn

GREASE employs a variety of linting tools, which can optionally be coordinated
via [Lūn]. We do our best to keep `lun.toml` up to date, but the ground truth of
which linters are run on what files and in what configuration is always supplied
by the CI system. To run them all:

[Lūn]: https://github.com/langston-barrett/lun

```sh
lun run
```

Lūn can run just the formatters, or run the linters in "fix" mode where
applicable. See the `--help` output for more information.

We recommend using this as a [pre-commit hook]:
```sh
cat <<'EOF' > .git/hooks/pre-commit
#!/usr/bin/env bash
lun run --check --staged
EOF
chmod +x .git/hooks/pre-commit
```

[pre-commit hook]: https://git-scm.com/docs/githooks#_pre_commit

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

## ttlint

We lint text files with [ttlint].

```sh
git ls-files -z --exclude-standard '*.cabal' '*.hs' '*.md' '*.py' '*.scala' | xargs -0 ttlint
```

[ttlint]: https://github.com/langston-barrett/ttlint

## typos

We run [typos] on `doc/` to spell-check the documentation. To run it locally,
try:

```bash
typos doc/
```

[typos]: https://github.com/crate-ci/typos

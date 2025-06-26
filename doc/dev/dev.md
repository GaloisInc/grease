# Development

## Build

To build and install from source, you'll need to install:

- GHC 9.4, 9.6, or 9.8
- `cabal-install` 3.6 or later

Follow the instructions on the [Haskell installation page][install-haskell].

[install-haskell]: https://www.haskell.org/downloads/

Then, clone GREASE and its submodules:

```sh
git clone https://github.com/GaloisInc/grease
cd grease
git submodule update --init
```

Then, build with `cabal`:

```sh
cabal build exe:grease
```

See [Test suite](tests.md) for how to run the tests.

## Docker

GREASE also offers a nightly Docker image that gets built after each commit to
the `main` branch. To run GREASE on an input using Docker, run the following

```sh
docker run ghcr.io/galoisinc/grease:nightly <input>
```

GREASE's test suite can also be run through Docker, although it requires
changing the entrypoint to use `grease-tests` instead:

```sh
docker run --entrypoint grease-tests ghcr.io/galoisinc/grease:nightly
```

The Docker image is available for both `amd64` and `arm64` architectures.

## Documentation

Documentation is built with [mdBook]. Install with `cargo` (or with a package
manager):

[mdBook]: https://rust-lang.github.io/mdBook/

```sh
cargo install mdbook
```

Then build the book with:

```sh
cd doc
mdbook build
```

As always, see `--help` for more options.

## Linting

### Generic scripts

We have a few Python scripts in `scripts/lint/` that perform one-off
checks. They generally take some number of paths as arguments, check
`.github/workflows/lint.yml` to see how they are invoked in CI.

### Fourmolu

This repo enforces Fourmolu formatting based on the committed configuration in `fourmolu.yaml`. Installation and usage directions are available
[here](https://github.com/fourmolu/fourmolu). In short:

```sh
cabal install fourmolu-0.18.0.0
fourmolu --mode inplace $(git ls-files '*.hs')
```

One can configure [auto-format on save](https://code.visualstudio.com/docs/editing/codebasics#_formatting) to avoid formatting issues.
The repo is already formatted with fourmolu so new formatting changes should be localized to behavioral changes. Further discussion of the rationale for the enforcement of a fourmolu style and commit hygiene is available in this [formatting discussion](./formatting.md).

To create a [pre-commit hook](https://git-scm.com/docs/githooks#_pre_commit):
```
cat <<'EOF' > .git/hooks/pre-commit
#!/usr/bin/env bash

# Run Fourmolu on changed files before committing, see `doc/dev/dev.md`

set -eu

files=$(git diff --name-only --cached -- '*.hs')
if [[ -n "${files}" ]]; then
    fourmolu --mode inplace "${files}"
    git add "${files}"
fi
EOF
chmod +x .git/hooks/pre-commit
```

#### Ignoring large repository reformats in git blame

Git blame is often helpful for determining commits related to changing a given line. Unfortunately, large scale code automated code changes (e.g. reformatting) can pollute the blame with a commit that is not functionally relevant to the given line. This repository maintains a `.git-blame-ignore-revs` to ignore this type of commit. The file lists past reformatting commits. To use this file to ignore these commits when running git blame configure git with the following command:

```sh
git config blame.ignorerevsfile .git-blame-ignore-revs
```

### hlint

We treat a small number of hlint warnings as errors in CI. To run hlint locally, try:

```sh
hlint grease{,-aarch32,-ppc,-x86}/src grease-cli/{main,src,tests}
```

### ruff

We lint and format the Python linting scripts and Ghidra plug-in with [ruff].

```sh
ruff format scripts/lint ghidra_scripts
ruff check scripts/lint ghidra_scripts
```

[ruff]: https://docs.astral.sh/ruff/

### typos

We run [typos] on `doc/` to spell-check the documentation. To run it locally,
try:

```bash
typos doc/
```

[typos]: https://github.com/crate-ci/typos

## Source code

The `grease` source code is split up into a number of smaller libraries, each
residing in its own top-level directory:

* `grease`: This comprises the core of `grease` as a library.
* `grease-aarch32`: This extends the `grease` library with the ability to reason
  about AArch32 binaries.
* `grease-ppc`: This extends the `grease` library with the ability to reason
  about 32-bit and 64-bit PowerPC binaries.
* `grease-x86`: This extends the `grease` library with the ability to reason
  about x86-64 binaries.
* `grease-cli`: This defines a command-line application on top of the library
  code in `grease`.

## Periodic tasks

The following is a list of things that should be done occasionally by GREASE
developers.

- Check for out-of-date documentation
- Fix warnings from `cabal haddock`
- Review and triage old issues
- Search for and fix broken [links to the GREASE documentation]
- Update versions of dependencies and tools
  - Bump bounds on Hackage dependencies (`cabal outdated` can help)
  - Bump submodules
  - Update versions of tools used in CI:
    - Cabal
    - GHC (see below)
    - Linters
    - OS images
    - `what4-solvers`
  - Update versions of tools used in the Dockerfile:
    - `ghcup`

[links to the GREASE documentation]: https://github.com/search?q=owner%3AGaloisInc+%2Fgaloisinc.github.io%5C%2Fgrease%5C%2F%2F&type=code

## GHC versions

We support the three most recent versions of GHC.
We try to support new versions as soon as they are supported by the libraries that we depend on.

### Adding a new version

GREASE has several Galois-developed dependencies that are pinned as Git submodules, in `deps/`.
These dependencies need to support new GHC versions before GREASE itself can.
First, create GitHub issues on each of these dependencies requesting support for the new GHC version.
Then, create an issue on the GREASE repo that includes:

1. A link to the GHC release notes for the new version
2. Links to the issues on the dependencies
3. A link to this section of this document

Then, wait for the issues on the dependencies to be resolved.
When adding support for the new GHC version to GREASE itself, complete the following steps:

- For each package:
  - [ ] Allow the [new version of `base`][base] in the Cabal `build-depends`
  - [ ] Run `cabal {build,test,haddock}`, bumping dependency bounds and submodules as needed
  - [ ] Fix any new warnings from [`-Wdefault`][wdefault]
- [ ] Add the new GHC version to the matrix in the GitHub Actions workflows
- [ ] Bump the GHC version in the Dockerfile to the latest supported version
- [ ] Follow the below steps to remove the oldest GHC version

[base]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history
[wdefault]: https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag-Wdefault

### Removing an old version

- [ ] Remove the old version from the matrix in the GitHub Actions configuration
- [ ] Remove outdated CPP `ifdef`s that refer to the dropped version
- [ ] Remove outdated `if` stanzas in the Cabal files

<!-- Copyright (c) Galois, Inc. 2024. -->

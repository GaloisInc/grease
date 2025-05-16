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

<!-- Copyright (c) Galois, Inc. 2024. -->

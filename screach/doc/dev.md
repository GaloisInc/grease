# Development

## Build

To build and install `screach` from source, you'll need to install:

- GHC 9.4, 9.6, or 9.8
- `cabal-install` 3.8 or later

Follow the instructions on the [Haskell installation page][install-haskell].

[install-haskell]: https://www.haskell.org/downloads/

Then, clone Screach and a few submodules:

```sh
git clone https://github.com/GaloisInc/grease
cd grease
git submodule update --init
```

Finally, build with `cabal`:

```sh
cabal build screach
```

See [Test suite](tests.md) for how to run the tests.

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

## Style

We use the same coding style [as the GREASE project][style].

[style]: https://galoisinc.github.io/grease/dev/style.html

## Linting

We use the same linting tools [as the GREASE project][lint]. See
`.gitlab-ci.yml` for exactly how each linter is run.

[lint]: https://galoisinc.github.io/grease/dev/lint.html

## Formatting

Screach uses fourmolu to format haskell code. [Fourmolu can be installed](https://github.com/fourmolu/fourmolu?tab=readme-ov-file#installation) with `cabal install fourmolu-0.19.0.1`

For setting up VS Code setting the formatting provider to fourmolu in the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) and [enabling autosave](https://code.visualstudio.com/docs/editing/codebasics#_formatting) should be sufficient.

Alternatively, in `.githooks/pre-commit` there is a [pre-commit hook](https://git-scm.com/docs/githooks#_pre_commit) script that runs fourmolu on each commit, adding this to .git/hooks/pre-commit should run format on every commit.

## Source code

The `screach` source code is split up into a number of individual libraries,
each residing in its own top-level directory:

* `screach`: The `screach` library and command-line tool.
* `elf-edit-ecfs`: A library for parsing
  [ECFS](https://github.com/elfmaster/ecfs) files into a form that
  tools like `screach` can reason about.

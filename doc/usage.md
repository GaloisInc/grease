# Usage

## Prerequisites

GREASE requires one of the following SMT solvers at run-time:

- `cvc4` 1.8 or greater
- `cvc5` 1.0.5 or greater
- `yices` 2.6.2 or greater
- `z3` 4.8.14 or greater

They are already installed in the GREASE Docker image. By default, `grease`
will default to `yices` unless you manually specify a solver using the
`--solver` option.

## Running a standalone binary

```sh
$ grease -- <filename>
```

By default, GREASE analyzes every known function in the input program. You can
use one of the following flags to specify specific entrypoints:

- `--symbol`: Analyze a function with a specific symbol name (e.g., `main`).
- `--address`: Analyze a function at a specific address.
- `--{symbol,address}-startup-override`: Like `--{symbol,address}`, but with a
  particular startup override. (For more information on startup overrides, see
  the [overrides](./overrides.md) section.)
- `--core-dump`: Analyze the function most likely to contain the address where
  the core was dumped in the supplied core dump file.

By default, GREASE will treat `<filename>` as an ELF file and read the ELF
headers to determine whether it is AArch32, PowerPC, or x86_64. This behavior
can be overridden if `<filename>` ends in one of the following suffixes:

- `.armv7l.elf`: 32-bit ARM ELF executable
- `.ppc32.elf`: 32-bit PowerPC ELF executable
- `.ppc64.elf`: 64-bit PowerPC ELF executable
- `.x64.elf`: x86_64 ELF executable
- `.bc`: LLVM bitcode

or one of the following, which are mostly useful for GREASE developers:

- `.armv7l.cbl`: 32-bit ARM CFG written in macaw-symbolic Crucible syntax
- `.ppc32.cbl`: 32-bit PowerPC CFG written in macaw-symbolic Crucible syntax
- `.x64.cbl`: x86_64 CFG written in macaw-symbolic Crucible syntax
- `.llvm.cbl`: LLVM CFG written in crucible-llvm syntax

(Note that `.ppc64.cbl` files are not currently supported, as `grease`'s 64-bit
PowerPC frontend currently requires a binary in order to work. See [this
upstream issue](https://github.com/GaloisInc/macaw/issues/415) for more
details.)

## Running the Docker image

```sh
$ docker run --rm -v "$PWD:$PWD" -w "$PWD" grease:latest <filename> --symbol <entrypoint>
```

<!-- Copyright (c) Galois, Inc. 2024. -->

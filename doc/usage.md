# Usage

This page describes the usage of the GREASE command-line interface.

## Running the Docker image

To run GREASE from the Docker image:

```sh
docker run grease:latest <args>
```

For `<args>`, see below.

Some helpful Docker flags:

- `--rm`: Delete the container after it exits
- `-v "$PWD:$PWD" -w "$PWD"`: Mount your current directory into the Docker image
  and set it as the working directory

All together:
```sh
docker run --rm -v "$PWD:$PWD" -w "$PWD" grease:latest <args>
```

## Running a standalone binary

```sh
grease -- <filename>
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

Try `--help` to see additional options.

## Running on a non-ELF image

GREASE has some support for analyzing non-ELF (i.e., raw) position dependent executables (e.g. firmware images)
by loading a file at a fixed offset. The `--raw-binary` flag enables this mode. By default the image
is loaded at offset `0x0`, which is likely not ideal for many binaries so an offset can be set with `--load-base BASE_ADDR`.
There are no symbols in a raw binary, so address entrypoints are the only valid entrypoints (i.e. `--address`).

## Path strategies

GREASE supports a few different *path strategies*, which indicate how to handle
branching. A strategy can be specified using `--path-strategy`.

- SSE stands for *static symbolic execution*. In this mode, GREASE always
  merges paths at control-flow join points.
- DFS stands for *depth-first search*. In this mode, GREASE never merges paths,
  and explores paths in a depth-first traversal.

<!-- Copyright (c) Galois, Inc. 2024. -->

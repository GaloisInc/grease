# ECFS Ghidra Extension

This Ghidra extension teaches Ghidra how to recognize and load
[`ee-ecfs`](https://github.com/elfmaster/ee-ecfs) snapshots.

## What it does

- Detects ECFS snapshots by reading the ELF header `EI_PAD` bytes for `ECFS`
- Applies Ghidra loading fixups that ECFS snapshots require:
  - treat `.text` as allocated/executable
  - suppress the synthetic `._TEXT` marker section so Ghidra does not load it as
    a second code block
- Imports ECFS-only extra symbol tables like `.dynsym.libc.so.6` into
  `ECFS::Symbols::<table>`
- Annotates thread PCs from `.prstatus` with comments and analysis bookmarks

After importing an ECFS coredump, you can tell that the plug-in has worked by
checking for the `ECFS prstatus` bookmark (type: "Analysis").

## Building

Set `GHIDRA_INSTALL_DIR` to your local Ghidra checkout or install directory and
run Gradle from this directory:

```sh
export GHIDRA_INSTALL_DIR=/path/to/ghidra
../ghidra-plugin/gradlew -p ../ghidra-ecfs buildExtension
```

(If you already have Gradle installed you can simply run `gradle
buildExtension`.)

To install the built extension directly into that Ghidra installation:

```sh
export GHIDRA_INSTALL_DIR=/path/to/ghidra
../ghidra-plugin/gradlew -p ../ghidra-ecfs installExtension
```

The extension zip will be written to `dist/`.

If you prefer to use Nix to install Gradle, you can build in a Nix shell
instead:

```sh
nix-shell --run 'gradle buildExtension'
```

The provided `shell.nix` sets `GHIDRA_INSTALL_DIR` to the Nix-packaged Ghidra.
If you want to build against a different local Ghidra checkout, override
`GHIDRA_INSTALL_DIR` after entering the shell.

## Development

<!-- [file:doc/dev/lint.md] -->
See [doc/dev/lint.md](../doc/dev/lint.md) for formatting and linting setup.

## Generating ECFS snapshots

The repo's `ecfs` branch contains a Docker-based test fixture for generating
`ee-ecfs` snapshots:

- `grease-exe/tests/feat/ecfs/Dockerfile`
- `grease-exe/tests/feat/ecfs/build.sh`
- `grease-exe/tests/feat/ecfs/generate-snapshot.sh`

That fixture produces a sample `test.x64.elf` ECFS snapshot which is useful for
manual import testing in Ghidra.

## Test suite

To run the tests:

```sh
cd screach
cabal test pkg:screach
```

The tests reside in the `screach/test-data/` directory. They are automatically
discovered by the test harness based on their file name. They are written using
[Oughta][oughta].

[oughta]: https://github.com/GaloisInc/oughta

We divide the tests into two general categories: (1) tests involving binaries
(which have an `*.elf` file extension) and (2) tests involving S-expression
files (which have a `*.cbl` file extension).

## Binary test cases

Binary test cases are compiled from C programs (which have the same file name,
but a `*.c` file extension). Use the Makefile in the `screach/test-data/` to
compile the C programs into binaries in a way that produces relatively
repeatable results across different machines.

C files may embed additional flags to pass to the C compiler using the special
comment syntax `// CFLAGS: `, e.g., `// CFLAGS: -g`.

The script that extracts these flags (`screach/test-data/extract-cflags.py`)
also supports flag *groups*, which start with `$`. For example, one flag group
`$CFLAGS_COMMON`, which expands to `-fno-stack-protector -nostartfiles`. See
the source code of the script for a full list of flag groups and explanations
of what each group does.

### Screach Directed Tests

Under the directory `screach/directed-test-data` there are binary test-cases that can be built cross platform (e.g. on arm64 machines) via crosstool-NG.
The directions for how to build the toolchain are in `test-gen`

## Lua API

The test harness provides the following Lua API, which enriches that of Oughta
with Screach-specific functionality:

- `prog`: The name of the program under test, e.g., `"test.cbl"`.
- `go(prog_name)`: Run Screach with the given string as the name of the program.
  Frequently, `prog` is passed as the argument, e.g., `go(prog)`. `go` may be
  called multiple times per test, see `global.c` for an example.
- `flags(fs)`: Append the given flags to the arguments passed to Screach when
  `go` is invoked. The flags are cleared after `go` is run.

The API is embedded in the comments of the source code for each test case. For
binaries, the API is used in the corresponding `*.c` file, and for S-expression
programs, the API is used in the `*.cbl` file itself. For instance, here is how
one would use the Lua API to invoke Screach on a binary containing a function
named `test` as the entrypoint, which calls a target function named `vuln`:

```c
// all: flags {"--entry-symbol", "test"}
// all: flags {"--target-symbol", "vuln"}
// all: go(prog)
```

void vuln(void) {}
void test(void) { vuln(); }

## Writing good tests

The Rust Compiler Development Guide has some [helpful guidance] on writing
high-quality tests. The Screach test suite is generally quite similar in
structure to that of rustc, so almost all of the advice there applies *mutatis
mutandis*.

[helpful guidance]: https://github.com/rust-lang/rust/blob/3350c1eb3fd8fe1bee1ed4c76944d707bd256876/src/doc/rustc-dev-guide/src/tests/best-practices.md

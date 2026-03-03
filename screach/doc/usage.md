# Usage

## Prerequisites

Screach requires a recent version of one of the following SMT solvers at
run-time: `cvc4`, `cvc5`, `yices`, or `z3`. Screach is currently tested against
`yices` version 2.6.2 in CI. Screach will default to using `yices` unless you
manually specify a solver using the `--solver` option.

## Running Screach

A minimal invocation of `screach` at the command line looks like the following:

```
$ screach PROG
    (--entry-addr ADDR | --entry-symbol SYMBOL)
    (--target-addr ADDR | --target-symbol SYMBOL)
```

That it, `screach` takes a program (`PROG`), an entrypoint location in the
program (either `--entry-addr` or `--entry-symbol`), and a target location in
the program (either `--target-addr` or `--target-symbol`). Screach will then
analyze the program to see if the target location is reachable from the
entrypoint location.

Some notes on the command-line options:

* `PROG` must be one of the following:

  * An x86-64 ELF binary.
  * An x86-64 [ECFS](https://github.com/elfmaster/ecfs) file.
  * An x86-64 CFG written in [Crucible S-expression syntax]. This is mostly
    useful for Screach developers. For more information about this feature, see
    [Writing S-expression programs](sexp.md).

* `ADDR` must be a valid address in the binary. Note that Crucible S-expression
  syntax programs lack addresses, and supplying an address as a location for an
  S-expression program will result in an error.
* `SYMBOL` must be a function name. For binaries, this typically corresponds to
  the symbol name of a function defined in the binary, although `SYMBOL` can
  also be a local function in an override file.

If `PROG` is not an ECFS core dump, then Screach will lack access to code from
shared libraries.

[Crucible S-expression syntax]: https://github.com/GaloisInc/crucible/tree/master/crucible-syntax

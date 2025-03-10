# Rust programs

`grease` has experimental support for analyzing Rust programs, both when
compiled to executables and when compiled to LLVM bitcode. From a `grease`
perspective, the average Rust program is significantly more complicated than,
say, an average C program for a variety of reasons, including:

* Rust has a large standard library, and much of the standard library will be
  included in a typical Rust program. As such, `grease` has to simulate more
  code on average.
* Some library functions (e.g., Rust functions related to string formatting
  or panicking) have rather complex implementations that are difficult to
  symbolically execute. As such, it is often necessary to
  [override](./overrides.md) tricky library functions to replace them with
  simpler implementations.
* The code that `rustc` produces often looks quite different from the code that
  a typical C compiler would produce. [`grease`'s memory
  model](./memory-model.md) is designed to work well with C compiler idioms,
  but perhaps not as much with `rustc` idioms.

As a general rule, it is wise to invoke `grease` with the `--rust` flag when
simulating Rust programs. Doing so changes some of `grease`'s simulator
settings to be more permissive when analyzing code so that it does not
mistakenly flag common `rustc` idioms as undefined behavior. This is especially
important for dealing with Rust enums, as `grease` will often flag enum-related
code as reading from uninitialized memory unless `--rust` is enabled.

<!-- Copyright (c) Galois, Inc. 2024. -->

# Overrides

Overrides are models of functions that can be called during symbolic execution.
Overrides can be used to model the effect of external functions, e.g., functions
from dynamically-linked libraries. They can also be used to replace the
definitions of complex functions in the target program with simpler models.

Some overrides are [built-in to `grease`](builtins.md), e.g., for some functions
from libc, such as `malloc`. Users may also provide overrides, which take
priority over both built-in overrides and functions defined in the program under
analysis.

User-provided overrides are written in the [Crucible S-expression
language](sexp.md).

## LLVM example

As an example, consider the following C program in the file `prog.c`:

```c
#include <stdio.h>
extern void *id_ptr(void*);
int main() {
  int x;
  int *y = id_ptr(&x);
  *y = 5;
  printf("%i\n", x);
  return 0;
}
```

If we compile this program to LLVM and disassemble it like so:[^flags]

```sh
clang -g -emit-llvm -O1 -c -fno-discard-value-names prog.c
llvm-dis prog.bc
```

Then the LLVM disassembly looks something like this:

```llvm
define i32 @main() {
entry:
  %x = alloca i32, align 4
  %0 = bitcast i32* %x to i8*
  %call = call i8* @id_ptr(i8* nonnull %0)
  ; -- snip --
  ret i32 0
}

; -- snip --

declare i8* @id_ptr(i8*)
```

Since `@id_ptr` is not accompanied by a definition, GREASE fails to execute
this function:

```
grease prog.bc

-- snip --
Failed to infer precondition.
```

If `id_ptr` is supposed to be the identity function on (64-bit) pointers, we
can write the following LLVM override in `id_ptr.llvm.cbl`:

```
(defun @id_ptr ((p (Ptr 64))) (Ptr 64)
  (start start:
    (return p)))
```

and pass it to GREASE, which can then successfully execute the target function:

```
grease prog.bc --overrides id_ptr.llvm.cbl

5
-- snip --
All goals passed!
```

Note that the `--overrides` flag may be passed multiple times (e.g.,
`--overrides foo.llvm.cbl --overrides bar.llvm.cbl`) to supply multiple
overrides.

[^flags]: The only flags that are strictly necessary here are `-c` and `-emit-llvm`, the other flags aid the readability of the LLVM code.

## Override file naming conventions

While GREASE does not enforce this convention, it is strongly encouraged to
name override files as follows:

<!-- This list also appears in sexp.md -->

- `*.armv7l.cbl` for AArch32 overrides
- `*.llvm.cbl` for LLVM overrides
- `*.ppc32.cbl` for PPC32 overrides
- `*.ppc64.cbl` for PPC64 overrides
- `*.x64.cbl` for x86_64 overrides

This naming scheme matches convention that GREASE requires for simulating
S-expression programs.

Each `<name>.*.cbl` file is required to define a function named `@<name>`.
This is referred to as the file's _public_ function, which is treated as an
override during simulation. All other functions defined in the file are
referred to as _auxiliary_ functions. Auxiliary functions are _not_ treated as
overrides during simulation, and they are effectively private to the file that
defines them.

## Built-in overrides

In addition to the user-defined overrides described above, GREASE also provides
a number of _built-in_ overrides that are built directly into the tool. Many of
the built-in overrides correspond to functions from libc. Unlike user-defined
overrides, these overrides are always enabled by default, without needing the
user to explicitly enable them.

If a user supplies an override of the same name as a built-in override, then the
user-supplied override takes precedence over the built-in override. User-defined
overrides may call built-in overrides.

A complete list can be found on [the page on built-in overrides](builtins.md).

## Overrides and forward declarations

It is possible for user-defined overrides to have _forward declarations_ to
other functions. For example, this override defines a `id_ptr2` function, which
invokes a forward declaration to `id_ptr`:

```
(declare @id_ptr ((p (Ptr 64))) (Ptr 64))

(defun @id_ptr2 ((p (Ptr 64))) (Ptr 64)
  (start start:
    (let res (funcall @id_ptr p))
    (return res)))
```

This `id_ptr2` override can then be combined with the `id_ptr` override above
like so:

```
grease prog.bc --overrides id_ptr.llvm.cbl --overrides id_ptr2.llvm.cbl
```

`grease` will ensure that the forward declaration to `id_ptr` is resolved to
the public function of the same name. If `grease` cannot find a public function
for an override named `id_ptr`, then `grease` will raise an error.

## Startup overrides

Startup overrides are a special form of override that runs before the execution
of an entrypoint function. A startup override can be thought of as overriding
the part of `grease` that creates the initial arguments that are passed to the
entrypoint function before simulating it. While `grease`'s default heuristics
can often figure out the right argument values to pass to the function, there
are times when it can be convenient to have finer-grained control over this
process. Some examples:

1. Triggering a bug requires an extremely large memory allocation. For instance,
   imagine that a bug is only triggered when a pointer argument points to an
   allocation of size `2^^64` bytes in size. Because `grease`'s default
   heuristics increase the size of allocations one byte at a time, a `grease`
   user would have a wait a very long time in order for the heuristics to
   discover that the memory allocation must be of size `2^^64` (assuming that
   `grease` does not time out or trigger its recursion limit before then).

2. Triggering a bug requires multiple arguments to be related to each other in a
   way that is not captured by `grease`'s heuristics. For example, it is
   somewhat commonplace for C functions to pass pairs of arguments where one
   argument is a buffer (call it `buf`) and another argument is the size of
   `buf` (call it `buf_sz`). One would expect that the size of the allocation
   that `buf` points to would always be equal to `buf_sz`, but `grease`'s
   heuristics do not guarantee this property: `grease` is liable to increase the
   size of `buf` in a way that does not respect the value of `buf_sz`.

In such scenarios, it can be helpful to use a startup override to carefully
dictate what the initial arguments to the function should be. To do so, one can
use the `--symbol-startup-override SYMBOL:FILE` or `--address-startup-override
ADDR:FILE` command-line arguments. These are like `--symbol` and `--address`,
respectively, except that they each take an additional `FILE` argument
(separated from the function symbol or address by a `:` character) representing
the path to the startup override file.

Like other forms of overrides, startup overrides are expected to be written in
Crucible S-expression syntax. They must also adhere to the following
constraints:

1. There must be a function named "`startup`" contained in the file. The
   `startup` function will be invoked at startup right before invoking the
   entrypoint function with the argument values that the startup override
   produces.

2. The type of the `startup` function has very particular requirements:

   * If the input program is a binary or a Macaw S-expression program, then
     `startup` must take a register struct as input and return a register struct
     as output. For example, on x86-64 the argument and result type must be
     `X86Regs`.

   * If the input program is LLVM bitcode or an LLVM S-expression program, then
     `startup` must take the same argument types as the entrypoint function,
     and it must return a struct containing the same argument types. For
     example, if the entrypoint function is an LLVM function with the type
     signature `i8 foo(i16, i32)`, then the startup override must have the type
     `(defun @startup ((x (Ptr 16)) (y (Ptr 32))) (Struct (Ptr 16) (Ptr 32))
     ...)`.

  In both cases, the arguments to the startup override consist of the "default"
  initial arguments that `grease` would normally pass directly to the
  entrypoint function, and the return value is what will _actually_ be passed
  to the entrypoint function.

Note that:

* Unlike other forms of overrides, startup overrides do not require their file
  paths to have particular naming conventions.

* Just like how the entrypoint function can be invoked multiple times during
  the [refinement](./refinement.md) process, the startup override can also be
  invoked multiple times during refinement. As a result, it is possible for the
  startup override to be supplied _different_ arguments each time it is is
  invoked during refinement.

<!-- Copyright (c) Galois, Inc. 2024. -->

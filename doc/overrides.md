# Overrides

Overrides are models of functions that can be called during symbolic execution.
Overrides can be used to model the effect of external functions, e.g., functions
from dynamically-linked libraries. They can also be used to replace the
definitions of complex functions in the target program with simpler models.

Some overrides are built-in to `grease`, e.g., for some functions from libc,
such as `malloc`. Users may also provide overrides, which take priority over
both built-in overrides and functions defined in the program under analysis.

User-provided overrides are written in the [Crucible S-expression syntax].
Each language supported by GREASE extends this base S-expression language with
additional types and operations, see [macaw-symbolic-syntax] for binaries and
[crucible-llvm-syntax] for LLVM for further information about these extensions.

[Crucible S-expression syntax]: https://github.com/GaloisInc/crucible/tree/master/crucible-syntax
[macaw-symbolic-syntax]: https://github.com/GaloisInc/macaw/tree/master/symbolic-syntax
[crucible-llvm-syntax]: https://github.com/GaloisInc/crucible/tree/master/crucible-llvm-syntax

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

## Override file naming conventions

While GREASE does not enforce this convention, it is strongly encouraged to
name override files as follows:

- `*.armv7l.cbl` for ARM overrides
- `*.llvm.cbl` for LLVM overrides
- `*.ppc32.cbl` for PPC32 overrides
- `*.x86.cbl` for x86_64 overrides

This naming scheme matches convention that GREASE requires for simulating
S-expression programs.

Each `<name>.*.cbl` file is required to define a function named `@<name>`.
This is referred to as the file's _public_ function, which is treated as an
override during simulation. All other functions defined in the file are
referred to as _auxiliary_ functions. Auxiliary functions are _not_ treated as
overrides during simulation, and they are effectively private to the file that
defines them.

[^flags]: The only flags that are strictly necessary here are `-c` and `-emit-llvm`, the other flags aid the readability of the LLVM code.

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

## Built-in overrides

In addition to the user-defined overrides described above, GREASE also provides
a number of _built-in_ overrides that are built directly into the tool. For
various reasons, these overrides are not defined using S-expression syntax.
Unlike user-defined overrides, these overrides are always enabled by default,
without needing the user to explicitly enable them. If a user supplies an
override of the same name as a built-in override, then the user-supplied
override takes precedence over the built-in override.

The following is a complete list of built-in overrides, given with C-like type
signatures:

<!-- ./scripts/get-overrides.sh --libc-overrides -->

- `i8* @memcpy( i8*, i8*, size_t )`
- `i8* @__memcpy_chk ( i8*, i8*, size_t, size_t )`
- `i8* @memmove( i8*, i8*, size_t )`
- `i8* @memset( i8*, i32, size_t )`
- `i8* @__memset_chk( i8*, i32, size_t, size_t )`
- `i8* @calloc( size_t, size_t )`
- `i8* @realloc( i8*, size_t )`
- `i8* @malloc( size_t )`
- `i32 @posix_memalign( i8**, size_t, size_t )`
- `void @free( i8* )`
- `i32 @printf( i8*, ... )`
- `i32 @__printf_chk( i32, i8*, ... )`
- `i32 @putchar( i32 )`
- `i32 @puts( i8* )`
- `size_t @strlen( i8* )`
- `double @ceil( double )`
- `float @ceilf( float )`
- `double @floor( double )`
- `float @floorf( float )`
- `float @fmaf( float, float, float )`
- `double @fma( double, double, double )`
- `i32 @isinf( double )`
- `i32 @__isinf( double )`
- `i32 @__isinff( float )`
- `i32 @isnan( double )`
- `i32 @__isnan( double )`
- `i32 @__isnanf( float )`
- `i32 @__isnand( double )`
- `double @sqrt( double )`
- `float @sqrtf( float )`
- `double @sin( double )`
- `float @sinf( float )`
- `double @cos( double )`
- `float @cosf( float )`
- `double @tan( double )`
- `float @tanf( float )`
- `double @asin( double )`
- `float @asinf( float )`
- `double @acos( double )`
- `float @acosf( float )`
- `double @atan( double )`
- `float @atanf( float )`
- `double @sinh( double )`
- `float @sinhf( float )`
- `double @cosh( double )`
- `float @coshf( float )`
- `double @tanh( double )`
- `float @tanhf( float )`
- `double @asinh( double )`
- `float @asinhf( float )`
- `double @acosh( double )`
- `float @acoshf( float )`
- `double @atanh( double )`
- `float @atanhf( float )`
- `double @hypot( double, double )`
- `float @hypotf( float, float )`
- `double @atan2( double, double )`
- `float @atan2f( float, float )`
- `float @powf( float, float )`
- `double @pow( double, double )`
- `double @exp( double )`
- `float @expf( float )`
- `double @log( double )`
- `float @logf( float )`
- `double @expm1( double )`
- `float @expm1f( float )`
- `double @log1p( double )`
- `float @log1pf( float )`
- `double @exp2( double )`
- `float @exp2f( float )`
- `double @log2( double )`
- `float @log2f( float )`
- `double @exp10( double )`
- `float @exp10f( float )`
- `double @__exp10( double )`
- `float @__exp10f( float )`
- `double @log10( double )`
- `float @log10f( float )`
- `void @__assert_rtn( i8*, i8*, i32, i8* )`
- `void @__assert_fail( i8*, i8*, i32, i8* )`
- `void @abort()`
- `void @exit( i32 )`
- `i8* @getenv( i8* )`
- `i32 @htonl( i32 )`
- `i16 @htons( i16 )`
- `i32 @ntohl( i32 )`
- `i16 @ntohs( i16 )`
- `i32 @abs( i32 )`
- `i32 @labs( i32 )`
- `i64 @labs( i64 )`
- `i64 @llabs( i64 )`
- `i32 @__cxa_atexit( void (i8*)*, i8*, i8* )`
- `i32 @open( i8*, i32 )`
- `i32 @close( i32 )`
- `ssize_t @read( i32, i8*, size_t )`
- `ssize_t @write( i32, i8*, size_t )`

For LLVM programs (but not binaries), the following built-in overrides are also
available:

<!-- ./scripts/get-overrides.sh --llvm-overrides -->

- `void @llvm.lifetime.start( i64, i8* )`
- `void @llvm.lifetime.end( i64, i8* )`
- `void @llvm.assume ( i1 )`
- `void @llvm.trap()`
- `void @llvm.ubsantrap( i8 )`
- `i8* @llvm.stacksave()`
- `void @llvm.stackrestore( i8* )`
- `void @llvm.memmove.p0i8.p0i8.i32( i8*, i8*, i32, i32, i1 )`
- `void @llvm.memmove.p0i8.p0i8.i32( i8*, i8*, i32, i1 )`
- `void @llvm.memmove.p0.p0.i32( ptr, ptr, i32, i1 )`
- `void @llvm.memmove.p0i8.p0i8.i64( i8*, i8*, i64, i32, i1 )`
- `void @llvm.memmove.p0i8.p0i8.i64( i8*, i8*, i64, i1 )`
- `void @llvm.memmove.p0.p0.i64( ptr, ptr, i64, i1 )`
- `void @llvm.memset.p0i8.i64( i8*, i8, i64, i32, i1 )`
- `void @llvm.memset.p0i8.i64( i8*, i8, i64, i1 )`
- `void @llvm.memset.p0.i64( ptr, i8, i64, i1 )`
- `void @llvm.memset.p0i8.i32( i8*, i8, i32, i32, i1 )`
- `void @llvm.memset.p0i8.i32( i8*, i8, i32, i1 )`
- `void @llvm.memset.p0.i32( ptr, i8, i32, i1 )`
- `void @llvm.memcpy.p0i8.p0i8.i32( i8*, i8*, i32, i32, i1 )`
- `void @llvm.memcpy.p0i8.p0i8.i32( i8*, i8*, i32, i1 )`
- `void @llvm.memcpy.p0.p0.i32( ptr, ptr, i32, i1 )`
- `void @llvm.memcpy.p0i8.p0i8.i64( i8*, i8*, i64, i32, i1 )`
- `void @llvm.memcpy.p0i8.p0i8.i64( i8*, i8*, i64, i1 )`
- `void @llvm.memcpy.p0.p0.i64( ptr, ptr, i64, i1 )`
- `i32 @llvm.objectsize.i32.p0i8( i8*, i1 )`
- `i32 @llvm.objectsize.i32.p0i8( i8*, i1, i1 )`
- `i32 @llvm.objectsize.i32.p0i8( i8*, i1, i1, i1 )`
- `i32 @llvm.objectsize.i32.p0( ptr, i1, i1, i1 )`
- `i64 @llvm.objectsize.i64.p0i8( i8*, i1 )`
- `i64 @llvm.objectsize.i64.p0i8( i8*, i1, i1 )`
- `i64 @llvm.objectsize.i64.p0i8( i8*, i1, i1, i1 )`
- `i64 @llvm.objectsize.i64.p0( ptr, i1, i1, i1 )`
- `void @llvm.prefetch.p0i8( i8*, i32, i32, i32 )`
- `void @llvm.prefetch.p0( ptr, i32, i32, i32 )`
- `void @llvm.prefetch( i8*, i32, i32, i32 )`
- `float @llvm.copysign.f32( float, float )`
- `double @llvm.copysign.f64( double, double )`
- `float @llvm.fabs.f32( float )`
- `double @llvm.fabs.f64( double )`
- `float @llvm.ceil.f32( float )`
- `double @llvm.ceil.f64( double )`
- `float @llvm.floor.f32( float )`
- `double @llvm.floor.f64( double )`
- `float @llvm.sqrt.f32( float )`
- `double @llvm.sqrt.f64( double )`
- `float @llvm.sin.f32( float )`
- `double @llvm.sin.f64( double )`
- `float @llvm.cos.f32( float )`
- `double @llvm.cos.f64( double )`
- `float @llvm.pow.f32( float, float )`
- `double @llvm.pow.f64( double, double )`
- `float @llvm.exp.f32( float )`
- `double @llvm.exp.f64( double )`
- `float @llvm.log.f32( float )`
- `double @llvm.log.f64( double )`
- `float @llvm.exp2.f32( float )`
- `double @llvm.exp2.f64( double )`
- `float @llvm.log2.f32( float )`
- `double @llvm.log2.f64( double )`
- `float @llvm.log10.f32( float )`
- `double @llvm.log10.f64( double )`
- `i1 @llvm.is.fpclass.f32( float, i32 )`
- `i1 @llvm.is.fpclass.f64( double, i32 )`
- `float @llvm.fma.f32( float, float, float )`
- `double @llvm.fma.f64( double, double, double )`
- `float @llvm.fmuladd.f32( float, float, float )`
- `double @llvm.fmuladd.f64( double, double, double )`
- `<2 x i64> @llvm.x86.pclmulqdq(<2 x i64>, <2 x i64>, i8)`
- `void @llvm.x86.sse2.storeu.dq( i8*, <16 x i8> )`

For binaries (but not LLVM programs), the following built-in overrides are also
available:

- `void @__stack_chk_fail()`
- `void @__stack_chk_fail_local()`

The types in the list above represent Crucible types according to the following
schema:

- `double`: (there is no syntax for this Crucible type)
- `float`: (there is no syntax for this Crucible type)
- `i<N>`: `(Ptr <N>)`
- `<<LANES> x i<N>>`: (there is no syntax for this Crucible type)
- `ptr`: `(Ptr <word-size>)`
- `size_t`: `(Ptr <word-size>)`
- `ssize_t`: `(Ptr <word-size>)`
- `<T>*`: `(Ptr <word-size>)`
- `void`: `Unit`

where `<word-size>` is:

- 32 for AArch32
- 64 for LLVM
- 32 for PPC32
- 64 for PPC64
- 64 for x86_64

They may be called from user-defined overrides.

The following overrides merit a bit of discussion:

- `abort`

  Terminate `grease` by raising an error. Note that this termination is
  immediate, so `grease` will not attempt to detect issues in any code following
  the call to `abort`.

- `free`

  Free a pointer that was allocated using `malloc`.

- `malloc`

  Allocate a fresh pointer of the given size in the underlying
  [memory model](./memory-model.md). This pointer is assumed not to alias with
  any pointers allocated by previous calls to `malloc`.

- `open`, `close`, `read`, and `write`

  These overrides leverage Crucible's experimental symbolic I/O capabilities.
  In particular, these overrides require the use of a symbolic filesystem,
  which must be specified with `--fs-root <path-to-filesystem-root>` when
  invoking `grease`. See [symbolic I/O] for a more detailed description of what
  the contents of the symbolic filesystem should look like.

  [symbolic I/O]: https://github.com/GaloisInc/crucible/tree/master/crux-llvm#symbolic-io-experimental

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

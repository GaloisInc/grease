# Built-in overrides

In addition to [user-defined overrides](overrides.md), GREASE also provides a
number of _built-in_ overrides that are built directly into the tool. Many of
the built-in overrides correspond to functions from libc. For various reasons,
these overrides are not defined using S-expression syntax. Unlike user-defined
overrides, these overrides are always enabled by default, without needing the
user to explicitly enable them.

If a user supplies an override of the same name as a built-in override, then the
user-supplied override takes precedence over the built-in override. User-defined
overrides may call built-in overrides.

## The list of built-in overrides

The following is a complete list of built-in overrides, given with C-like type
signatures.

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

### LLVM-specific overrides

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

### Binary-specific overrides

For binaries (but not LLVM programs), the following built-in overrides are also
available:

- `void @__stack_chk_fail()`
- `void @__stack_chk_fail_local()`

### S-expression-specific overrides

For LLVM S-expression files (programs or overrides), the following overrides are also available:
```
(declare @read-bytes ((x (Ptr 64))) (Vector (Bitvector 8)))
(declare @read-c-string ((x (Ptr 64))) (String Unicode))
(declare @write-bytes ((dest (Ptr 64)) (src (Vector (Bitvector 8)))) Unit)
(declare @write-c-string ((dst (Ptr 64)) (src (String Unicode))) Unit)
```

See [the upstream documentation](https://github.com/GaloisInc/crucible/blob/master/crucible-llvm-syntax/README.md#string-manipulation).

### Discussion

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

## Crucible types for overrides

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

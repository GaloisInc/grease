# The S-expression language

This page describes how GREASE uses the the [Crucible S-expression language].
There are two ways that GREASE interacts with this language:

1. Users can provide [overrides](overrides.md) written in it.
2. Developers can write test-cases in it, see [S-expression programs](sexp-progs.md).

Each language supported by GREASE extends the base S-expression language with
additional types and operations, see:

- [macaw-symbolic-syntax] for binaries
  - [macaw-aarch32-syntax] for AArch32
  - [macaw-ppc-syntax] for PowerPC
  - [macaw-x86-syntax] for x86_64
- [crucible-llvm-syntax] for LLVM

[Crucible S-expression language]: https://github.com/GaloisInc/crucible/tree/master/crucible-syntax
[macaw-symbolic-syntax]: https://github.com/GaloisInc/macaw/tree/master/symbolic-syntax
[macaw-aarch32-syntax]: https://github.com/GaloisInc/macaw/tree/master/macaw-aarch32-syntax
[macaw-ppc-syntax]: https://github.com/GaloisInc/macaw/tree/master/macaw-ppc-syntax
[macaw-x86-syntax]: https://github.com/GaloisInc/macaw/tree/master/macaw-x86-syntax
[crucible-llvm-syntax]: https://github.com/GaloisInc/crucible/tree/master/crucible-llvm-syntax

## S-expression-specific overrides

There are a few overrides that are only available in S-expression files
(programs or overrides).

### Concretization overrides

GREASE provides two kinds of concretization overrides:

**Unsound concretization** (`conc-*`): Makes 1 solver query to get a concrete
value. Fast but unsound for verification; picks an arbitrary value when
multiple solutions exist.

**Sound (unique) concretization** (`unique-conc-*`): Makes 2 solver queries to
check if the value has exactly one possible model. If unique, concretizes;
otherwise returns the symbolic value unchanged. Sound for verification.

```
(declare @conc-bool ((b Bool)) Bool)
(declare @conc-bv-8 ((bv (Bitvector 8))) (Bitvector 8))
(declare @conc-bv-16 ((bv (Bitvector 16))) (Bitvector 16))
(declare @conc-bv-32 ((bv (Bitvector 32))) (Bitvector 32))
(declare @conc-bv-64 ((bv (Bitvector 64))) (Bitvector 64))
(declare @conc-integer ((i Integer)) Integer)
(declare @conc-nat ((n Nat)) Nat)
(declare @conc-ptr-32 ((p (Ptr 32))) (Ptr 32))
(declare @conc-ptr-64 ((p (Ptr 64))) (Ptr 64))
(declare @conc-vector-bv-8 ((v (Vector (Bitvector 8)))) (Vector (Bitvector 8)))
(declare @conc-vector-bv-16 ((v (Vector (Bitvector 16)))) (Vector (Bitvector 16)))
(declare @conc-vector-bv-32 ((v (Vector (Bitvector 32)))) (Vector (Bitvector 32)))
(declare @conc-vector-bv-64 ((v (Vector (Bitvector 64)))) (Vector (Bitvector 64)))

(declare @unique-conc-bool ((b Bool)) Bool)
(declare @unique-conc-bv-8 ((bv (Bitvector 8))) (Bitvector 8))
(declare @unique-conc-bv-16 ((bv (Bitvector 16))) (Bitvector 16))
(declare @unique-conc-bv-32 ((bv (Bitvector 32))) (Bitvector 32))
(declare @unique-conc-bv-64 ((bv (Bitvector 64))) (Bitvector 64))
(declare @unique-conc-integer ((i Integer)) Integer)
(declare @unique-conc-nat ((n Nat)) Nat)
(declare @unique-conc-ptr-32 ((p (Ptr 32))) (Ptr 32))
(declare @unique-conc-ptr-64 ((p (Ptr 64))) (Ptr 64))
(declare @unique-conc-vector-bv-8 ((v (Vector (Bitvector 8)))) (Vector (Bitvector 8)))
(declare @unique-conc-vector-bv-16 ((v (Vector (Bitvector 16)))) (Vector (Bitvector 16)))
(declare @unique-conc-vector-bv-32 ((v (Vector (Bitvector 32)))) (Vector (Bitvector 32)))
(declare @unique-conc-vector-bv-64 ((v (Vector (Bitvector 64)))) (Vector (Bitvector 64)))
```

#### `@conc-bool`, `@conc-integer`, `@conc-nat`, `@conc-bv-*`

These overrides request a model from the SMT solver and return the concrete
value of the input in the model.

#### `@unique-conc-bool`, `@unique-conc-integer`, `@unique-conc-nat`, `@unique-conc-bv-*`

These overrides check if the value has a unique concrete value across all
satisfying models. If unique, returns the concrete value; otherwise returns
the symbolic value unchanged.

#### `@conc-ptr-*`

These overrides request a model from the SMT solver and return a pointer with
both the block number and offset concretized in the same model.

#### `@unique-conc-ptr-*`

These overrides check if the pointer has a unique block number and offset
across all satisfying models. If unique, returns the concrete pointer;
otherwise returns the symbolic pointer unchanged.

#### `@conc-vector-bv-*`

These overrides request a model from the SMT solver and return a vector where
all bitvector elements have been concretized in the same model. This ensures
consistency across all elements of the vector.

#### `@unique-conc-vector-bv-*`

These overrides check if all elements of the vector have unique concrete values
across all satisfying models. If all elements are uniquely determined, returns
the concrete vector; otherwise returns the symbolic vector unchanged.

### `@fresh-bytes`

The bytes created using `fresh-bytes` will be concretized and printed to the
terminal if GREASE finds a possible bug.

`fresh-bytes` can be used to create overrides for functions that do I/O, such
as the following basic override for `recv`, shown for x86_64 and LLVM:

```
; Basic x86_64 override for `recv`.
;
; Ignores `socket` and `flags`, always reads exactly `length` bytes.

(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
(defun @recv ((socket (Ptr 64)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 64))) (Ptr 64)
  (registers
    ($ctr (Bitvector 64))  ; loop counter (going down)
    ($idx Nat)             ; index into vector of bytes
    ($ptr (Ptr 64)))       ; pointer to write the next byte into
  (start start:
    (let length-bv (pointer-to-bits length))
    (let bytes (funcall @fresh-bytes "recv" length-bv))

    (set-register! $ctr length-bv)
    (set-register! $idx 0)
    (set-register! $ptr buffer)
    (jump loop:))
  (defblock loop:
    (let byte (vector-get bytes $idx))
    (pointer-write (Bitvector 8) le $ptr byte)

    (set-register! $ctr (- $ctr (bv 64 1)))
    (set-register! $idx (+ $idx 1))
    (let ptr (pointer-add $ptr (bv 64 1)))
    (set-register! $ptr ptr)
    (branch (equal? $ctr (bv 64 0)) end: loop:))
  (defblock end:
    (return length)))
```
```
; Basic LLVM override for `recv`.
;
; Ignores `socket` and `flags`. Always reads exactly `length` bytes.

(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
(defun @recv ((socket (Ptr 32)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 32))) (Ptr 64)
  (registers
    ($ctr (Bitvector 64))  ; loop counter (going down)
    ($idx Nat)             ; index into vector of bytes
    ($ptr (Ptr 64)))       ; pointer to write the next byte into
  (start start:
    (let length-bv (ptr-offset 64 length))
    (let bytes (funcall @fresh-bytes "recv" length-bv))

    (set-register! $ctr length-bv)
    (set-register! $idx 0)
    (set-register! $ptr buffer)
    (jump loop:))
  (defblock loop:
    (let byte (vector-get bytes $idx))
    (let byte-ptr (ptr 8 0 byte))
    (store none i8 $ptr byte-ptr)

    (set-register! $ctr (- $ctr (bv 64 1)))
    (set-register! $idx (+ $idx 1))
    (let ptr (ptr-add-offset $ptr (bv 64 1)))
    (set-register! $ptr ptr)
    (branch (equal? $ctr (bv 64 0)) end: loop:))
  (defblock end:
    (return length)))
```

### LLVM-specific overrides

For LLVM S-expression files (programs or overrides), the following overrides are also available:
```
(declare @read-bytes ((x (Ptr 64))) (Vector (Bitvector 8)))
(declare @read-c-string ((x (Ptr 64))) (String Unicode))
(declare @write-bytes ((dest (Ptr 64)) (src (Vector (Bitvector 8)))) Unit)
(declare @write-c-string ((dst (Ptr 64)) (src (String Unicode))) Unit)
```

See [the upstream documentation](https://github.com/GaloisInc/crucible/blob/master/crucible-llvm-syntax/README.md#string-manipulation).

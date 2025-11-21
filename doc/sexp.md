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
```
(declare @conc-bv-8 ((bv (Bitvector 8))) (Vector (Bitvector 8)))
(declare @conc-bv-16 ((bv (Bitvector 16))) (Vector (Bitvector 16)))
(declare @conc-bv-32 ((bv (Bitvector 32))) (Vector (Bitvector 32)))
(declare @conc-bv-64 ((bv (Bitvector 64))) (Vector (Bitvector 64)))
(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector w))) (Vector (Bitvector 8)))
```

### `@conc-bv-*`

These overrides request a model from the SMT solver and return the value of the
input bitvector in the model.

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

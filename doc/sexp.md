# Writing S-expression programs

In addition to binaries and LLVM bitcode files, GREASE supports analysis of
programs written in several variants of the [Crucible S-expression language].
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

## File naming conventions

<!-- This list also appears in overrides.md -->

S-expression programs must be named as follows:

- `*.armv7l.cbl` for AArch32 syntax
- `*.llvm.cbl` for LLVM syntax
- `*.ppc32.cbl` for PPC32 syntax
- `*.ppc64.cbl` for PPC64 syntax
- `*.x64.cbl` for x86_64 syntax

## Conventions for entrypoints

Entrypoints of non-LLVM S-expression programs must take a single argument
and return a single value, both of a designated architecture-specific struct
type, representing the values of all registers. These struct types are called
`AArch32Regs`, `PPC32Regs`, `PPC64Regs`, and `X86Regs`.

For example, here is a minimal AArch32 S-expression program that swaps the
values in `R0` and `R1`:
```
(defun @test ((regs0 AArch32Regs)) AArch32Regs
  (start start:
    (let init-r0 (get-reg r0 regs0))
    (let init-r1 (get-reg r1 regs0))
    (let regs1 (set-reg r0 init-r1 regs0))
    (let regs2 (set-reg r1 init-r0 regs1))
    (return regs2)))
```

For more information about this struct, see [the Macaw documentation].

[the Macaw documentation]: https://github.com/GaloisInc/macaw/blob/master/doc/Design.md#translation

## S-expression-specific Overrides

There are a few overrides that are only available in S-expression files
(programs or overrides).
```
(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector w))) (Vector (Bitvector 8)))
```

The bytes created using `fresh-bytes` will be concretized and printed to the
terminal if GREASE finds a possible bug. `fresh-bytes` can be used to create
overrides for functions that do I/O, such as the following basic override for
`recv`:
```
; Basic override for `recv`.
;
; Ignores `socket` and `flags`. Always reads exactly `length` bytes.

(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
(defun @recv ((socket (Ptr 32)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 32))) (Ptr 64)
  (start start:
    (let length-bv (ptr-offset 64 length))
    (let bytes (funcall @fresh-bytes "recv" length-bv))
    (let byte (vector-get bytes 0))
    (let byte-ptr (ptr 8 0 byte))
    (store none i8 buffer byte-ptr)
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

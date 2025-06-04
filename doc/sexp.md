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

## Additional operations

GREASE provides the following operations in S-expression files, in addition to
the operations provided by the base S-expression language:

- `fresh-vec :: String Unicode -> forall (t :: Type) -> Nat -> Vector t`, where
  `(fresh-vec s t n)` generates a length-`n` vector where each element is a
  fresh constant of type `t` with the name `<s>_<i>` (for each `i` between 0 and
  `<n> - 1`). Note that `t` must be a scalar type (e.g., no nested `Vector`s),
  and `s` and `n` must both be concrete values.

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

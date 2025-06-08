# S-expression programs

In addition to ELF binaries, GREASE can analyze standalone programs written
in [the Crucible S-expression language](sexp.md). This is mostly useful for
developers in writing test-cases for GREASE.

## File naming conventions

<!-- This list also appears in overrides.md -->

Standalone S-expression programs must be named as follows:

- `*.armv7l.cbl` for AArch32 syntax
- `*.llvm.cbl` for LLVM syntax
- `*.ppc32.cbl` for PPC32 syntax
- `*.ppc64.cbl` for PPC64 syntax
- `*.x64.cbl` for x86_64 syntax

[Overrides](overrides.md) follow a similar convention.

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

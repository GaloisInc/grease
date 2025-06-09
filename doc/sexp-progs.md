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

## Register names

Each extension to the Crucible S-expression language has its own documentation,
but for the sake of convenience we reproduce the register naming schemes here:

- [AArch32][macaw-aarch32-syntax]:
  - General purpose registers: `r0`, ..., `r10`, `fp` (AKA `r11`), `ip` (AKA `r12`), `sp` (AKA `r13`), and `lr` (AKA `r14`)
  - Floating-point registers: `v0`, ..., `v31`
- [PowerPC][macaw-ppc-syntax]:
  - General purpose registers: `ip`, `lnk`, `ctr`, `xer`, `cr`, `fpscr`, `vscr`, `r0`, ..., `r31`
  - Floating-point registers: `f0`, ..., `f31`
- [x86_64][macaw-x86-syntax]:
  - General purpose registers: `rip`, `rax`, `rbx`, `rcx`, `rdx`, `rsp`, `rbp`, `rsi`, `rdi`, `r8`, ..., `r15`
  - Floating-point registers: (no syntax)

Each extension exports `get-reg` and `set-reg` operations, as shown above for AArch32.

[macaw-aarch32-syntax]: https://github.com/GaloisInc/macaw/tree/master/macaw-aarch32-syntax
[macaw-ppc-syntax]: https://github.com/GaloisInc/macaw/tree/master/macaw-ppc-syntax
[macaw-x86-syntax]: https://github.com/GaloisInc/macaw/tree/master/macaw-x86-syntax

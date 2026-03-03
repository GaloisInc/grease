# Overrides

Overrides are models of functions that can be called during symbolic execution.
Overrides can be used to model the effect of external functions, e.g., functions
from dynamically-linked libraries. They can also be used to replace the
definitions of complex functions in the target program with simpler models.

Some overrides are [built-in to `screach`](builtins.md), e.g., for some functions
from libc, such as `malloc`. Users may also provide overrides, which take
priority over both built-in overrides and functions defined in the program under
analysis.

User-provided overrides are written in the [Crucible S-expression
language](sexp.md).

Overrides in Screach work very similarly to those in GREASE, see [the GREASE
documentation on overrides][groverrides] for further information.

[groverrides]: https://galoisinc.github.io/grease/overrides.html

## Target overrides

A target override is an [address override] that uses the [`@reached` built-in]
to establish a more restrictive definition of a target than that of a plain
address (as with `--target-addr`) or function name (as with `--target-symbol`).
Using `@reached` within an target override, you can define a target that will
only be considered reachable if certain conditions on the contents of registers
or memory are met.

For example, the following target override can be used to check that `rcx` holds
the value `0xdeadbeef`:
```
(declare @reached ((x Bool)) Unit)
(defun @addr-ov ((regs X86Regs)) Unit
  (start start:
    (let rcx (get-reg rcx regs))
    (let deadbeef (bits-to-pointer (bv 64 0xdeadbeef)))
    (let b (pointer-eq rcx deadbeef))
    (funcall @reached b)
    (return ())))
```

[address override]: https://galoisinc.github.io/grease/overrides.html#address-overrides
[`@reached` built-in]: ./builtins.md#@reached

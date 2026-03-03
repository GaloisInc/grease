# Writing S-expression programs

In addition to binaries, Screach supports analysis of programs written in the
[Crucible S-expression language]. Specifically, it leverages the
[macaw-symbolic-syntax] and [macaw-x86-syntax] libraries to extend the base
S-expression language with additional types and operations. See those
libraries' respective `README`s for more information.

[Crucible S-expression language]: https://github.com/GaloisInc/crucible/tree/master/crucible-syntax
[macaw-symbolic-syntax]: https://github.com/GaloisInc/macaw/tree/master/symbolic-syntax
[macaw-x86-syntax]: https://github.com/GaloisInc/macaw/tree/master/macaw-x86-syntax

## Conventions for S-expression programs

S-expression programs must have the file extension `.cbl`. Also note that
unlike binaries, an S-expression program lacks addresses, which means that the
only locations that `screach` supports analyzing are the names of functions
(via the `--{entry,target}-symbol` command-line options). Attempting to analyze
addresses in an S-expression program (via the `--{entry,target}-addr`
command-line options) will result in an error.

An entrypoint function in an S-expression program must take a single argument
of type `X86Regs` and return a single value of types `X86Regs`. The `X86Regs`
type represents the values of all x86-64 registers. For example, here is a
minimal S-expression program that swaps the values in the first two argument
registers (`rdi` and `rsi`):

```
(defun @test ((regs0 X86Regs)) X86Regs
  (start start:
    (let init-rdi (get-reg rdi regs0))
    (let init-rsi (get-reg rsi regs0))
    (let regs1 (set-reg rdi init-rsi regs0))
    (let regs2 (set-reg rsi init-rdi regs1))
    (return regs2)))
```

For more information about this struct, see [the Macaw documentation].

[the Macaw documentation]: https://github.com/GaloisInc/macaw/blob/master/doc/Design.md#translation

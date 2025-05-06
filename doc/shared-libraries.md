# Shared libraries

`grease` has rudimentary support for binaries that dynamically link against
shared libraries, but with some notable caveats.

## Skipping external function calls

Dynamically linked binaries can invoke external functions defined in shared
libraries (e.g., `libc`). Unless `grease` has [an override](./overrides.md)
available, `grease` treats any invocation of a function defined in a shared
library as as no-op. `grease` will log that the function is skipped, but it
will otherwise not simulate it at all. If you want to intercept `grease`'s
behavior when it invokes an external function and have it do something,
consider supplying an override for the function.

## Limitations of PLT stub detection

On the architectures that `grease` supports, `grease` identifies which
functions are external by determining if the function is being invoked from a
_PLT stub_. Typically, a PLT stub is located in a specially marked `.plt` or
`.plt.sec` section of the binary.

Unfortunately, reliably identifying PLT stubs [is
hard](https://github.com/GaloisInc/macaw/issues/375). Therefore, `grease` has
(fallible) heuristics for determining which functions have PLT stubs, and it
relies on these heuristics to determine which functions are external (and
therefore should be skipped or not).

Unfortunately, `grease`'s PLT stub heuristics are not perfect. In particular,
`grease` does not have reliable heuristics for detecting PLT stubs that are
located in `.plt.got` sections, which are common in x86-64 binaries. If you run
`grease` on a binary with a PLT stub that `grease` cannot detect, then you will
likely get an error message stating that `grease` does not support the
relocation type associated with the PLT stub, e.g.,

```
grease: Attempted to read from an unsupported relocation type (R_X86_64_GLOB_DAT) at address 0x3ff0.
This may be due to a PLT stub that grease did not detect.
If so, try passing --plt-stub <ADDR>:<NAME>, where
<ADDR> and <NAME> can be obtained by disassembling the
relevant PLT section of the binary
(.plt, .plt.got, .plt.sec, etc.).
```

If this occurs, then you must supply the address and name of the PLT stub to
`grease` yourself. You can do so by passing `--plt-stub <ADDR>:<NAME>` to
`grease` on the command line, once for each PLT stub. (If you do not know the
name of the PLT stub, it is fine to leave `<NAME>` empty.) Reverse engineering
suites such as [Ghidra](https://ghidra-sre.org/) can be effective at
determining PLT stub information.

<!-- Copyright (c) Galois, Inc. 2024. -->

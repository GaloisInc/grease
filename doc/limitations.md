# Limitations

GREASE is subject to a variety of limitations. They can be characterized according to several criteria:

- Are the issues *fundamental*, i.e., inherent to the overall approach? Or are they *incidental*, and could be solved given enough engineering?
- Are they threats to *correctness*, i.e., could they induce GREASE to report a falsehood?
- Are they threats to *completeness*, i.e., could they cause GREASE to fail to analyze (part of) a given program, or to only analyze part of its behavior?

The following table characterizes several of GREASE's limitations, which are described in more detail below.

| Limitation             | Fundamental | Correctness | Completeness |
| ---------------------- | ----------- | ----------- | ------------ |
| Path explosion         | Y           | N           | Y            |
| *k*-bounding           | Y           | N           | Y            |
| External code          | Y           | Y           | Y            |
| Scalability of SMT     | Y           | N           | Y            |
| Analyzing binaries     | Y           | Y           | N            |
| Aliasing               | N           | Y           | Y            |
| Code discovery         | N           | N           | Y            |
| Heuristics             | N           | Y           | Y            |
| Features of binaries   | N           | N           | Y            |
| Quirks of machine code | N           | N           | Y            |

## Fundamental limitations

The limitations described in this section are fundamental to GREASE's analysis, and are rooted in theorems in computability and complexity theory. No amount of engineering effort could result in fully general, automated solutions to these problems. However, real progress can be made by introducing heuristics for common cases, or resorting to human insight and input.

### Path explosion

Like many program analyses, GREASE suffers from the *path explosion problem*:

> *path explosion* is a fundamental problem that limits the scalability and/or completeness of certain kinds of program analyses, including fuzzing, symbolic execution, and path-sensitive static analysis. Path explosion refers to the fact that the number of control-flow paths in a program grows exponentially ("explodes") with an increase in program size and can even be infinite in the case of programs with unbounded loop iterations. Therefore, any program analysis that attempts to explore control-flow paths through a program will [...] have exponential runtime in the length of the program[^path explosion]

[^path explosion]: https://en.wikipedia.org/wiki/Path_explosion

GREASE's under-constrained symbolic execution (uc-symex) attempts to mitigate the impact of path explosion relative to traditional symbolic execution (symex). By starting at an arbitrary function (instead of only at `main`, as in traditional symex), GREASE can achieve higher coverage of the program under analysis. However, uc-symex is still subject to path explosion: it still involves exploring all control-flow paths that start at the function under analysis. In the limit (i.e., when analyzing `main`), this can boil down to the same problem as in traditional symex.

In practice, this means that GREASE may report a timeout when analyzing functions with complex control-flow (or functions that call such functions).

### *k*-bounding

GREASE uses a sophisticated, bit-precise [memory model](memory-model.md) with support for advanced features such as reads, writes, and copies of symbolic data with symbolic sizes, to and from symbolic pointers. However, this memory model can't express *unbounded heap data-structures*. For example, GREASE can't determine the behavior of a function when fed a linked list of arbitrary length, it can only analyze the behavior on linked lists with symbolic content but some fixed length. In practice, GREASE examines program behavior up to some maximum *bound*, an approach known in static analysis as *k*-bounding. The program features subject to *k*-bounding include data structures, loop iterations, and recursion.

Practically speaking, GREASE may report that the bound *k* has been exceeded when analyzing complex functions. Users may set the bound *k* at the command-line.

### External code

Programs are not pure functions; they interact with the world. They call external libraries, interact with the kernel via syscalls, and query and manipulate with hardware devices. Not only is it infeasible to precisely model the plethora of complex systems with which a program under analysis might interface, given the precision of GREASE's analysis, it is often impractical to even soundly over- or under-approximate the behaviors of such systems.

GREASE provides precise models for a [wide variety of commonly used library functions](overrides.md#built-in-overrides) (e.g., from libc) and a limited set of Linux syscalls. However, there will always be many more that cannot be analyzed. GREASE skips such calls by default, but the user may also provide their own models where necessary in the form of [overrides](overrides.md).

### Scalability of SMT

Symbolic execution fundamentally relies on SMT solvers. Despite decades of incredible research and engineering effort, the performance of such solvers remains unpredictable from problem to problem. At Galois, we've seen performance degrade especially when reasoning about programs with heavy use of floating-point values, non-linear arithmetic, and certain patterns of memory usage.[^smt] To mitigate these difficulties, GREASE allows users to select among many SMT solvers.

If the SMT solver fails to answer a query, GREASE reports this as a timeout and doesn't offer further results for that function.

[^smt]: In addition to these practical limitations, there’s also a more fundamental issue: SMT solvers are built to solve an NP-hard problem. Therefore, their performance can’t be uniformly good in all cases.

### Analyzing binaries

GREASE operates on compiled code (binaries and LLVM). There may be bugs (e.g., undefined behavior) in the source program that the compiler has optimized away; GREASE has no means of finding such bugs. See [Undefined behavior](undefined-behavior.md) for a more detailed discussion of which undefined behaviors GREASE can check for.

## Incidental limitations

The following limitations reflect the state of GREASE today, but considerable progress on them is likely possible with further engineering effort.

### Aliasing

GREASE assumes that any two pointers reachable from a function's inputs (i.e., the values in registers/arguments) do not alias. It could likely be extended to consider such cases.

### Code discovery

Code discovery consists of recovering control-flow graphs (CFGs) from disassembled machine code. This task is undecidable in general, challenging in practice, and amounts to an active area of research in the field of binary analysis. GREASE's approach is thus necessarily heuristic in nature, and could always be improved to support additional common idioms (e.g., different kinds of jump tables emitted by major compilers).

If the code discovery fails to recover a CFG, GREASE reports as much and doesn't offer further results for control-flow paths that reach the site of the failure.

### Heuristics

Under-constrained symbolic execution (uc-symex) analyzes functions in isolation. As discussed above, this approach improves scalability as compared to traditional symex (by mitigating path explosion), but it comes with its own trade-offs. Crucially, it makes it difficult to say with certainty whether a behavior observed during analysis is feasible in practice. Any potential error encountered during analysis could either be a true bug, or an artifact of the analysis approach. Tools based on uc-symex have *heuristics* that attempt to maximize true positives and minimize false positives reported to the user. Such heuristics can generally be improved with additional investment.

### Features of binaries

Binary analysis is hard. Executable formats, operating systems, ISAs, relocation types, and memory models proliferate. GREASE currently supports analysis of Linux ELF binaries for 32-bit ARM, PowerPC, and x86_64. Even within those categores, GREASE lacks support for:

- [some types of relocations](https://github.com/GaloisInc/grease/issues/22)
- [executing code from shared libraries](https://github.com/GaloisInc/grease/issues/21)
- semantics for some instructions on some architectures (e.g., [`hlt` on x86_64](https://github.com/GaloisInc/grease/issues/10))

### Quirks of machine code

It is possible for machine code to exhibit behaviors that are not possible at the level of assembly language; GREASE cannot analyze such code. For example, GREASE doesn't support:

- self-modifying code
- run-time code generation (e.g., JIT compilation)
- jumps into the "middle" of instructions

### In brief

- GREASE can only analyze code in a single thread of execution, without considering side-effects from threads or processes acting in parallel.
- GREASE has limited support for string operations. For instance, calls to `strlen` [will only work on concrete pointers](https://github.com/GaloisInc/crucible/blob/bd78bbfc5bb495a30c1755d50f702c21c4266051/crucible-llvm/src/Lang/Crucible/LLVM/MemModel.hs#L1042-L1046). These operations are very difficult to handle in a performant way in full generality, as each additional character introduces a symbolic branch.
- GREASE can't handle [calls through non-concrete function pointers](https://github.com/GaloisInc/crucible/issues/10).
- GREASE [has a hard time with variable-arity functions](memory-model.md#the-stack).
- GREASE [cannot model heap exhaustion or overflows](memory-model.md#the-heap).
- It's [difficult to detect PLT stubs](shared-libraries.md#limitations-of-plt-stub-detection).
- GREASE does not [currently](https://github.com/GaloisInc/grease/issues/33) detect stack buffer overflows.
- GREASE does not currently support code that runs before `main`, e.g., C's `__attribute__ ((constructor))`, ELF's `.init`/`.ctors`/`.init_array`, LLVM's [`@llvm.global_ctors`][llvm-global-ctors], or Rust's [`_init`][rust-init].
- GREASE currently does not cope well with calls to symbolic function pointers or addresses (or syscalls with symbolic numbers). GREASE's default behavior is to skip over calls to function pointers or addresses that it cannot resolve as concrete. GREASE also includes a `--error-symbolic-fun-calls` command-line option that causes GREASE to error if it attempts to invoke a symbolic function call.

[llvm-global-ctors]: https://llvm.org/docs/LangRef.html#the-llvm-global-ctors-global-variable
[rust-init]: https://users.rust-lang.org/t/generating-a-cdylib-crate-without-an-init/85267

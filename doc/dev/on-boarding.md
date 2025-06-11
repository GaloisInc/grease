# On-boarding

This page provides learning resources for new GREASE developers.

## Binary analysis

### General background

- [GDB in The Architecture of Open-Source Applications](https://aosabook.org/en/v2/gdb.html)
- [Phrack](https://phrack.org/)
- [Eli Bendersky's website](https://eli.thegreenplace.net/)
  - [Posts on debuggers](https://eli.thegreenplace.net/tag/debuggers)
  - [Posts on assembly](https://eli.thegreenplace.net/tag/assembly)
    - [Load-time relocation of shared libraries](https://eli.thegreenplace.net/2011/08/25/load-time-relocation-of-shared-libraries)
    - [Position Independent Code (PIC) in shared libraries](https://eli.thegreenplace.net/2011/11/03/position-independent-code-pic-in-shared-libraries)
    - [Stack frame layout on x86-64](https://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64)
- [Binary symbolic execution with KLEE-Native](https://blog.trailofbits.com/2019/08/30/binary-symbolic-execution-with-klee-native/)

### Try it out!

- [ ] Use bintools
  - [ ] Read [`objdump` tldr], disassemble something!
  - [ ] Read [`readelf` tldr], examine an ELF file!
  - [ ] Read [`nm` tldr], look at some symbols!
- [ ] - Use `llvm-dwarfdump`
- [ ] Download Ghidra, look at an ELF program

[`objdump` tldr]: https://tldr.inbrowser.app/pages/common/objdump
[`readelf` tldr]: https://tldr.inbrowser.app/pages/common/readelf
[`nm` tldr]: https://tldr.inbrowser.app/pages/common/nm

## LLVM

- [Docs](https://llvm.org/docs/)
- [LLVM in the Architecture of Open-Source Applications](https://aosabook.org/en/v1/llvm.html)
- A [helpful blog](https://blog.yossarian.net/tags#llvm)
- Another [helpful blog](https://eli.thegreenplace.net/tag/llvm-clang)
- Yet one more [helpful blog](https://duckduckgo.com/?q=site%3Ahttps%3A%2F%2Fblog.regehr.org%2F+llvm&ia=web)
- Are there seriously [more blogs](https://www.npopov.com/)?

### Try it out!

- [ ] Compile a program to .ll with `clang -fno-discard-value-names -emit-llvm -grecord-gcc-switches -S -O1 test.c`
- [ ] Do the same thing using [compiler explorer](https://godbolt.org/)
- [ ] Assemble it to .bc with `llvm-as test.bc`, disassemble it again with `llvm-dis`

## Under-constrained symbolic execution

- [Blog post about an older tool that did this for LLVM](https://www.galois.com/articles/under-constrained-symbolic-execution-with-crucible)
- Original paper about UC-KLEE: [Under-Constrained Symbolic Execution: Correctness Checking for Real Code](https://www.usenix.org/conference/usenixsecurity15/technical-sessions/presentation/ramos)
- Follow-up paper: [Sys: A Static/Symbolic Tool for Finding Good Bugs in Good (Browser) Code](https://www.usenix.org/conference/usenixsecurity20/presentation/brown)

## Dependently-typed Haskell

GREASE and its underlying libraries are programmed using depedently-typed
Haskell.

### Singletons

This is the name of the approach of combining GADTs and type families to achieve
depedently-typed programming. The main paper is [Dependently typed programming
with singletons](https://dl.acm.org/doi/abs/10.1145/2430532.2364522).

The singletons library uses Template Haskell where parameterized-utils
encourages a more explicit style.

### parameterized-utils

This is our core dependently-typed programming library.

- [GitHub](https://github.com/GaloisInc/parameterized-utils/)
- [API documentation on Hackage](https://hackage.haskell.org/package/parameterized-utils)
- [Simply-typed lambda calculus example](https://github.com/robdockins/param-tlc)

### Idioms

See [Fancy Haskell idioms](haskell-idioms.md) for some examples of idioms that
arise when doing "fancy" Haskell programming.

### Try it out!

[Exercises](https://github.com/i-am-tom/haskell-exercises)

## Libraries

GREASE makes use of several libraries for binary analysis and symbolic execution.

### Macaw

Macaw performs code discovery: given some set of entrypoints (generally
addresses of functions, generally from the symbol table), Macaw recovers the
CFGs (bounds of and edges between basic blocks) and translates them into its own
IR. Macaw CFGs can then be turned into Crucible CFGs for symbolic execution.

- arXiv paper: <https://www.arxiv.org/pdf/2407.06375>
- [API documentation](https://galoisinc.github.io/macaw/)
- [GitHub](https://github.com/GaloisInc/macaw) (warning: not well documented)
- [Memory model blog post](https://www.galois.com/articles/making-a-scalable-smt-based-machine-code-memory-model)

The closest thing out there to the Macaw/Crucible combo is [angr](https://angr.io/).

#### Try it out!

Prepare to build Macaw (see the latest build instructions in the
[README](https://github.com/GaloisInc/macaw?tab=readme-ov-file#building)):
```sh
git clone https://github.com/GaloisInc/macaw
cd macaw
git submodule update --init
ln -s cabal.project.dist cabal.project
```
Then print the Macaw CFGs in a test ELF file:
```sh
cabal run exe:macaw-x86-dump -- discover x86_symbolic/tests/pass/identity.opt.exe
```
Pass `--crucible` to see the Crucible CFGs too.

### Crucible

Crucible is an imperative IR. It has an SSA format with basic blocks. It uses
dependently-typed programming to enforce well-typedness and the SSA invariant.
It comes with a symbolic execution engine. There are many frontends that
translate other languages into the Crucible IR, e.g., LLVM (e.g., from C and
C++ via Clang), JVM (e.g., from Java), MIR (e.g., from Rust), WebAssembly, and
crucially for us, machine code (via Macaw).

- [GitHub](https://github.com/GaloisInc/crucible)
- [API documentation on Hackage](https://hackage.haskell.org/package/crucible)
- [IR examples](https://github.com/GaloisInc/crucible/tree/master/crucible-cli/test-data/simulate)

#### Try it out!

First, install Yices and Z3. Then, Prepare to build
Crucible (see the latest build instructions in the
[README](https://github.com/GaloisInc/crucible?tab=readme-ov-file#quick-start)):
```sh
git clone https://github.com/GaloisInc/crucible
cd crucible
git submodule update --init
```
The run Crucible on some example IR files:
```sh
cabal run exe:crucible -- simulate crucible-cli/test-data/simulate/assert.cbl
```

### What4

What4 is our internal representation of symbolic terms. What4 values are
manipulated by Crucible programs. What4 wraps a variety of SMT solvers to make
queries about terms.

- [GitHub](https://github.com/GaloisInc/what4)
- [API documentation on Hackage](https://hackage.haskell.org/package/what4)

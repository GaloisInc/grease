# Glossary

This page attempts to define and especially disambiguate specialized terms that
may arise when reading about or developing GREASE.

## Control-flow graphs

"CFG" stands for "control-flow graph". See:

- [The rustc developer guide](https://rustc-dev-guide.rust-lang.org/appendix/background.html#what-is-a-control-flow-graph)
- [Wikipedia](https://en.wikipedia.org/wiki/Control-flow_graph)

In the context of GREASE, there are a few different kinds of CFGs:

- Macaw CFGs, which arise from code discovery: [`Data.Macaw.CFG.Core`]
- Crucible CFGs, which can be symbolically executed:
  - In SSA form: [`Lang.Crucible.CFG.Core`]
  - In "registerized" form: [`Lang.Crucible.CFG.Reg`]

The `macaw-symbolic` package transforms Macaw CFGs into Crucible CFGs.

Similarly, "expression" or "statement" may refer to expressions or statements in
any of the above CFGs.

[`Data.Macaw.CFG.Core`]: https://galoisinc.github.io/macaw/Data-Macaw-CFG-Core.html
[`Lang.Crucible.CFG.Core`]: https://hackage-content.haskell.org/package/crucible/docs/Lang-Crucible-CFG-Core.html
[`Lang.Crucible.CFG.Reg`]: https://hackage-content.haskell.org/package/crucible/docs/Lang-Crucible-CFG-Reg

## Memory

In the context of GREASE, "memory" may refer to:

- Actual run-time memory (i.e., RAM) of a program under analysis
- Macaw memory, which is a pre-loader representation of the memory layout
  extracted from metadata in a binary: [`Data.Macaw.Memory`]
- The Crucible-LLVM memory model [`Lang.Crucible.LLVM.MemModel`], which serves
  as the representation of memory during symbolic execution.
- GREASE's memory model, which is essentially that of Crucible-LLVM mediated by
  the Macaw Crucible language extension. See [Memory model](../memory-model.md).

[`Data.Macaw.Memory`]: https://galoisinc.github.io/macaw/Data-Macaw-Memory.html
[`Lang.Crucible.LLVM.MemModel`]: https://hackage-content.haskell.org/package/crucible-llvm/docs/Lang-Crucible-LLVM-MemModel.html

## Common abbreviations and acronyms

- AArch32: 32-bit ARM ISA
- ABI: Application binary interface
- AMD64: Another name for x86_64
- Armv7l: A particular instantiation of AArch32
- ASL: ARM Semantics Language
- bc: LLVM bitcode
- CFG: Control-flow graph, see above
- ctx: "Context", sometimes [`Data.Parameterized.Context`]
- DWARF: This is not an acronym, but is a binary format for debugging information designed for use with ELF
- ELF: Executable and Linkable Format
- GHA: GitHub Actions
- GHC: Glasgow Haskell compiler
- GOT: Global offset table
- IR: Intermediate representation, usually of a compiler (e.g., LLVM IR)
- ISA: Instruction set architecture, e.g., AArch32 or x86_64
- ld: Loader (or linker)
- ll: Human readable format for LLVM IR, as opposed to LLVM bitcode
- LLVM: This is not an acronym. See <https://llvm.org/>.
- PLT: Procedure linkage table
- PPC: PowerPC
- reloc: ELF relocation
- rtld: Run-time loader
- SMT: Satisfiability modulo theories
- symex: Symbolic execution
- SysV: System V ABI, the ABI of x86_64 Linux
- uc-symex: Under-constrained symbolic execution

[`Data.Parameterized.Context`]: https://hackage-content.haskell.org/package/parameterized-utils/docs/Data-Parameterized-Context.html

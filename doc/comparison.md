# Comparison

| Tool    | Binary | Source Code/LLVM | Available Under-constrained Algorithm |
|---------|--------|------------------|---------------------------------------|
| [angr](https://angr.io/)    | Yes    | No*              | Yes*                                   |
| [Grease](https://github.com/GaloisInc/grease)  | Yes    | Yes              | Yes                                   |
| [KLEE](https://github.com/klee/klee)  | No     | Yes              | No                                    |
| UC-KLEE | No     | Yes              | Yes                                   |
| [UC-Crux](https://www.galois.com/articles/under-constrained-symbolic-execution-with-crucible) | No     | Yes              | Yes                                   |
| [Macaw](https://github.com/GaloisInc/macaw)   | Yes    | No               | No                                    |

Notes: *angr has some support for JVM bytecode although the extent of support is unclear. 
Similarly there is a UNDER_CONSTRAINED_SYMEXEC option that populates unbound pointers with a fresh region sufficient for the load
it is unclear how well this interacts with angr's concretizing memory model (e.g. pointers to regions with pointers etc)*

Grease focuses on providing a seamless out of the box under-constrained symbolic execution tool that allows analysts to find and fix bugs. The tool provides a "it just works" (subject to [limitations](limitations.md)) command line interface that allows engineers to directly symbolically execute arbitrary functions within a target binary or LLVM program and find bugs. This capability arises from a unique combination of a focus on scalable and relatively automated techniques (underconstrained symbolic execution), use of a generic [symbolic exeuction framework and memory model](https://github.com/GaloisInc/crucible/blob/master/crucible-llvm/doc/memory-model.md) that enables support for both binaries and LLVM programs, and wrapping these capabilities in an easy to use interface.

Below we highlight some general categories of tools that offer points of comparison

* **Binary analysis frameworks**: [angr](https://angr.io/), [Macaw](https://github.com/GaloisInc/macaw), [BAP](https://github.com/BinaryAnalysisPlatform/bap), and [binsec](https://github.com/binsec/binsec). These frameworks are typically libraries that provide core binary analysis capabilities. Example capabilities include disassembly, control flow recovery, lifting and simplification, variable recovery, and type inference. Frameworks often include symbolic semantics for some intermediate representation. Macaw and angr both support forward symbolic execution of binaries from an entrypoint. This approach on its own suffers from path explosion in complex programs, typically requiring the user to configure the analysis in some way for programs of moderate scale. In binary anaysis frameworks, this configuration is typically done by configuring the library manually with some interesting state and direction which requires expert analysis. These frameworks are also limited to performing analysis directly on a binary, preventing these tools from taking advantage of additional structure provided by source code (for instance types and segmented memory models).

* **Source analyzers/Symbolic execution engines**: [KLEE](https://github.com/klee/klee) performs forward symbolic execution on LLVM bitcode. Performing analysis on bitcode allows KLEE to use more effecient memory models that split memory into separate abstract objects. Unfortunately, the tight integration of KLEE with LLVM means that KLEE cannot perform analysis on binaries. The tool also suffers from path explosion on large programs when exploring from an entrypoint.

* **Source-level Underconstrained Symbolic Execution Engines**: [UC-Crux](https://www.galois.com/articles/under-constrained-symbolic-execution-with-crucible) and UC-KLEE perform underconstrained symbolic execution on LLVM bitcode, enabling automated symbolic execution of arbitrary functions within a target binary with minimal configuration. Due to the use of LLVM, these tools are limited to programs where the source code is available and can be compiled to LLVM bitcode.   


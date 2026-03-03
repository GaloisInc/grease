# Overview

Screach (**s**ymbolic **c**ompositional **reach**ability) is a command-line
tool, Ghidra plug-in, and Haskell library for performing reachability analysis
on binaries using symbolic execution. Screach supports analysis of ELF
executables containing x86_64 code.

The inputs to Screach are:

- An x86_64 ELF binary (or [ECFS] coredump)
- An entrypoint location from which to begin analysis
- A target location

[ECFS]: https://github.com/elfmaster/ecfs

where the entrypoint and target locations are either symbols or addresses.
Screach attempts to find a feasible control-flow path from the entrypoint to
the target.

Under the hood, Screach uses sophisticated program analysis techniques,
including under-constrained symbolic execution, directed symbolic execution
(specifically, shortest-distance symbolic execution), and an optional pointer
analysis.

<!-- TODO(internal#158): Simple demo -->

## Use-cases

Static analysis tools can produce valuable insights, but they are often buried
among false positives. You can triage such reports using Screach, to see if the
possible error is feasible in practice.

Bug reports often point to a failing assertion or memory access, but lack
concise inputs that reproduce the behavior. In such cases, Screach can help by
*synthesizing* inputs that reach the bug.

Services such as [Dependabot alerts] can report vulnerabilities in parts of
your dependencies that aren't actually utilized by your application. You can use
Screach to see if the vulnerable code is reachable from your application.

[Dependabot alerts]: https://docs.github.com/en/code-security/tutorials/secure-your-dependencies/dependabot-quickstart-guide?search-overlay-input=dependabot#viewing-dependabot-alerts-for-your-repository

## Shortest-distance symbolic execution

Screach performs [*directed symbolic execution*], specifically shortest-distance
*symbolic execution* (SDSE). SDSE requires as input an inter-procedural
control-flow graph. At every symbolic branch point (i.e., `if` with a
non-concrete condition), Screach suspends execution and pushes both branches
onto a worklist. It pops the branch from the worklist that has the shortest
distance in the interprocedural control-flow graph from the target location, and
continues exploring.

[*directed symbolic execution*]: https://link.springer.com/chapter/10.1007/978-3-642-23702-7_11

## Under-constrained symbolic execution

Screach can start its analysis from any function entrypoint in the binary. To
set up an initial symbolic state, Screach performs [*under-constrained symbolic
execution*] (UC-symex). UC-symex is a technique for iteratively refining an
initial symbolic state when errors occur so that exploration can continue and
cover more paths.

[*under-constrained symbolic execution*]: https://www.usenix.org/conference/usenixsecurity15/technical-sessions/presentation/ramos

<!-- This list is taken from the GREASE docs -->

The algorithm is roughly as follows. Screach symbolically executes the
entrypoint using the most general possible preconditions (all registers hold
fresh symbolic variables). In this process, it will add paths to the worklist
as described above. If an out-of-bounds memory read or write occurs, the
precondition is refined by growing allocated / initialized memory. For example,
if an out-of-bounds read occurs at an offset of the address in `r8`, we refine
the precondition to make sure that `r8` points to a region of initialized
memory big enough to contain the offset. Screach then restarts such paths from
the entrypoint with the refined precondition and *replays* their execution  to
the location where the error occurred. At that point, it pushes them to the
worklist. This process of exploration and refinement repeats itself until either
(1) the target location is reached, (2) no errors occur, or (3) errors occur
and we do not have heuristics to refine based on any of the errors. In this last
case, Screach continues exploring the paths already in the worklist.

Screach uses [GREASE] (as a library) for its implementation of UC-symex.

[GREASE]: https://galoisinc.github.io/grease/

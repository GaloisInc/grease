# Memory model

GREASE builds on the [Macaw][macaw] and [Crucible-LLVM][crucible] memory models.

[macaw]: https://github.com/GaloisInc/macaw
[crucible]: https://github.com/GaloisInc/crucible

## Background: Crucible-LLVM

In [the Crucible-LLVM memory model][crucible-llvm-mem] ([permalink][crucible-llvm-mem-perma]),

[crucible-llvm-mem]: https://github.com/GaloisInc/crucible/blob/master/crucible-llvm/doc/memory-model.md
[crucible-llvm-mem-perma]: https://github.com/GaloisInc/crucible/blob/3bdafe4fe1ed03e49a4255082cfe4d2e4a524f4b/crucible-llvm/doc/memory-model.md

> Each allocation in the memory model is completely isolated from all other
> allocations and has a unique block identifier. Pointers into the memory model
> have two components: the identifier of the block they reference and an offset
> into that block. Note that both the identifier and the offset can be symbolic.

Said another way, the memory model is *two-dimensional*: One "axis" of a
pointer determines which allocation (possibly allocations in the case of a
symbolic block identifier) it points to, and the other "axis" is the offset
within that allocation.[^bv] For example, the null pointer is represented by
the pair `(0, 0x0)`.

<!-- regenerate with:
cabal run exe:grease-diagrams -- --output doc/mem.svg --width 400 --height 400
-->

![Crucible-LLVM memory](mem.svg)

## Global memory

The [Macaw memory model][macaw-mem] (and so, GREASE) represents a binary's
address space as a single Crucible-LLVM allocation of size `0xffffffff`.
Pointers that appear in the binary become offsets into this allocation. For
example, if the global allocation had block identifier `1`, the global `g`
was at address `0x903278`, then a load from `g` would become a load from the
Crucible-LLVM pointer `(1, 0x903278)`.

[macaw-mem]: https://galois.com/blog/2023/03/making-a-scalable-smt-based-machine-code-memory-model/

### Mutable globals

Mutable global variables are tricky:

- If GREASE initialized mutable global variables to their initial values or
  to completely symbolic values, it's possible that GREASE would report "bugs"
  that are infeasible in practice, because the value of the global required to
  trigger the bug isn't actually feasible at any of the callsites to the
  function under analysis.
- On the other hand, if GREASE doesn't initialize global variables at all, any
  loads from them (that aren't preceded by writes made by the function under
  analysis or its callees) will fail. As mutable global variables are pervasive,
  this would lead to a significant lack of coverage.

GREASE's behavior in this regard is controlled via the `--globals` flag. The
possible values are:

- `initialized`: Each mutable global is initialized using its initializer before
  analysis of the target function.
- `symbolic`: Each mutable global is initialized to a symbolic value before
  analysis of the target function.
- `uninitialized`: Mutable globals are left uninitialized. Reads from mutable
  globals will fail, causing GREASE to be unable to proceed with analysis.

## The stack

GREASE represents the stack as a separate Crucible-LLVM allocation. Before
symbolically executing the target function, the stack pointer is initialized to
point at *the end* of a fairly large allocation (currently, 1 MiB). This means
that GREASE assumes a standard stack discipline, namely that the stack grows
downwards from high addresses to low, and functions never access memory above
their stack frame (with some exceptions, which are explained below). In theory,
false positives may occur if a binary overruns this stack allocation, though in
practice, symbolic execution of that much code would take a considerable amount
of time.

On some architectures, GREASE will allocate a small amount of space just above
the stack frame to store the current function's return address (e.g., on x86-64
and PowerPC) or the back chain (e.g., PowerPC), but otherwise, GREASE will
assume that functions never access memory above their stack frame. Note that
because GREASE leaves most of the space above the stack frame unallocated by
default, this will cause GREASE to reject programs that load function arguments
that are spilled to the stack.[^spilled] In general, it is challenging to infer
how many arguments a function has from analyzing raw assembly code, so GREASE
does not make an effort to do so.

Instead, GREASE offers a `--stack-argument-slots=<NUM>` that can be used to
allocate `<NUM>` additional stack slots above the stack frame before simulating
the entrypoint function. `<NUM>` is 0 by default, so if you know ahead of time
how many stack-spilled arguments the entrypoint function requires, you should
use `--stack-argument-slots` to ensure that GREASE can simulate it properly.
Note that in general, it is not advisable to run GREASE with a large
`--stack-argument-slots` value. This is because if GREASE reserves more stack
space than what is required to model stack-spilled arguments, then reading from
the extra stack space will succeed (incorrectly) instead of resulting in an
uninitialized stack read error (which is normally what is desired).

## The heap

GREASE overrides the behavior of the `malloc` and `free` functions such that
they interact nicely with the Crucible-LLVM memory model. Each call to `malloc`
will generate a distinct allocation with a non-zero block identifier that is
different from all other allocations in the program, including global memory.
Similarly, each call to `free` will deallocate a Crucible-LLVM allocation.

Be mindful of the following limitations of GREASE's model of the heap:

- There is no limit to the maximum heap size, so GREASE cannot model heap
  overflows.
- Because each heap allocation is completely isolated from all other
  allocations, GREASE cannot model events that would arise after certain types
  of heap-related exploits (e.g., buffer overflows).

[^bv]: Non-pointer bitvectors (e.g. the 5 in `int x = 5;`) are represented the same way as pointers, but with a block identifier that’s concretely 0.

[^spilled]: The number of arguments a function must have in order for it to
spill some of its arguments to the stack depends on which architecture is being
used. In general, the more arguments a function has, the more likely it is to
spill arguments to the stack.

<!-- Copyright (c) Galois, Inc. 2024. -->

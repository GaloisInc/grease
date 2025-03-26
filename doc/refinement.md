# Refinement

Before continuing, it would be helpful to review:

- the [overview](overview.md)
- the [memory model](memory-model.md)

This page describes the refinement algorithm in additional detail. However,
it does not discuss implementation techniques, which are documented [in
the Haddocks][haddocks].

[haddocks]: https://github.com/GaloisInc/grease/blob/main/grease/src/Grease/Refine.hs

The algorithm is similar to [the corresponding functionality in UC-Crux-LLVM][uc-crux].

[uc-crux]: https://galois.com/blog/2021/10/under-constrained-symbolic-execution-with-crucible/

The goal of the refinement loop is to discover sufficient preconditions for the
successful execution of a target function. If a memory error occurs
(out-of-bounds read/write, uninitialized read, etc.), the precondition is
refined using heuristics, such as expanding allocations or initializing memory.

## Preconditions

The preconditions are represented as a map from registers to *pointer shapes*.
A pointer shape consists of a sequence of *pointee shapes*, where a pointee
shape is one of:

- A number of uninitialized bytes
- A number of initialized bytes
- A pointer shape

In the initial precondition, all registers (except the instruction and stack
pointers) map to an empty sequence of pointee shapes, meaning that they are not
assumed to point to an allocation (i.e., they are treated as raw bitvectors).

## Setting up state

Before each execution of the target function, GREASE initializes memory and
registers according to the current precondition.

### Memory

As described in [the document on the memory model](memory-model.md), GREASE
maps the binary's address space into a single Crucible-LLVM allocation, and
sets the stack pointer to the end of a separate allocation.

<!-- TODO(lb): Are constant global variables initialized to their initial
values? What about mutable ones? -->

### Registers

All registers are set to symbolic values corresponding to their shape
(other than the stack and instruction pointers, which point to the end of an
allocation and the beginning of the function, respectively).

For each empty pointer shape, the register is set to a fresh, symbolic value.
For each non-empty pointer shape, GREASE creates a separate Crucible-LLVM
allocation that is just large enough to hold all of its pointee shapes, and
sets the register to a pointer to the base of that allocation. It then
initializes the allocation according to the pointee shapes (which may create
and initialize additional allocations, recursively).

## The refinement loop

The refinement loop proceeds by initializing memory and registers according to
the current precondition, then symbolically simulating the target function. If
no error occurs, a sufficient precondition has been found and the refinement
loop ends. If instead there is a memory error, the error is passed along to a
series of heuristics. The heuristics generally:

- Increase the size of pointee shapes (by adding uninitialized bytes at the end)
- Turn uninitialized pointee shapes into initialized ones
- Replace (un)initialized pointee shapes with pointer shapes

These heuristics can be disabled with the `--no-heuristics` flag.

<!-- Copyright (c) Galois, Inc. 2024. -->

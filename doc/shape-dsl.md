# Shape DSL

The goal of GREASE's [refinement](refinement.md) loop is to discover sufficient
preconditions for the successful execution of a target function. These
preconditions are represented as a mapping from arguments (i.e.,
registers) to *shapes*.

## Abstract syntax

Conceptually, a shape has the following grammar:

```
shape ::=
  | bv
  | bv-lit byte+
  | ptr memshape*

memshape ::=
  | uninit N
  | init N
  | exactly byte+
  | memptr memshape*

byte ::= 00 | 01 | ... | ff
```

Where:

- `bv` represents a symbolic bitvector of the appropriate width
- `bv-lit byte+` represents a concrete bitvector literal
- `ptr` represents a pointer, the `memshape*`s are what it points to
- `uninit N` represents `N` uninitialized bytes of memory
- `init N` represents `N` bytes of memory initialized to symbolic bytes
- `exactly byte+` represents a concrete sequence of bytes
- `memptr` represents a pointer to a further allocation

At certain verbosity levels, GREASE may output these preconditions in its logs.
It does so using a concrete syntax described below.

## Concrete syntax

Bitvectors (`bv`) are represented with sequences of `XX` (each representing a
symbolic byte), like so:

- 32-bit: `XX XX XX XX`
- 64-bit: `XX XX XX XX XX XX XX XX`

Bitvector literals (`bv-lit`) are written in hexadecimal, 0-padded on the left
to the appropriate width, and optionally prefixed with `0x`. They must have an
even number of hexadecimal digits.

- 32-bit: `0bad1dea`, `0x0bad1dea`
- 64-bit: `0bad1deadeadbeef`, `0x0bad1deadeadbeef`

Similarly, symbolic bytes (`init`) are written as sequences of `XX`s, separated
by spaces. Uninitialized bytes are written `##`. Concrete bytes (`exactly`) are
written like bitvector literals, in hexadecimal, 0-padded on the left to a width
of 2. Pointers (`ptr`, `memptr`) are represented as an *allocation number* plus
an *offset*. The targets of pointers are shown out-of-line.

The following example shows a pointer in AArch32 register `R1`. The pointer
points to allocation number `00`, at offset `4`. Allocation `00` contains four
uninitialized bytes, followed by two initialized, symbolic bytes, followed by
the concrete bytes `fe` and `0a`, followed by a pointer to allocation `01`.
Allocation `01` doesn't contain anything at all (thus, any read from or write to
it would fail).

```
R1: 00+00000004
00: ## ## ## ## XX XX fe 0a 01+00000000
01:
```

For 32-bit architectures, the allocation numbers are `0`-padded on the left to
a width of 2, whereas for 64-bit architectures, they are padded to a width of
6. The offsets are padded to the maximal number of hex digits required to write
numbers of that bit-width, namely 8 for 32-bit and 16 for 64-bit. This scheme
ensures that pointers have the same textual width as a pointer-width sequence
of bytes.

Long, repeated sequences of symbolic or uninitialized bytes can be written with
*run-length encoding*. For example, a sequence of 64 uninitialized bytes would
be `##*00000040`. Just as for pointer offsets, the run length is represented as
a hexadecimal number of the size of the machine bit-width.

## Specifying initial preconditions

GREASE begins its analysis with a "minimal" precondition,[^minimal] and uses
its heuristics to search for a precondition that avoids errors. In many cases,
GREASE's heuristics cannot find such a precondition. In such cases, you can
manually specify an initial precondition from which to begin refinement.

### Example

For example, consider this (contrived) program:
```c
// test.c
#include <stdlib.h>

void test(int *p, int *q) {
  if (*p == 0) {
    // do some stuff
    free(q);
  }
  // some other stuff
  free(q);
}

int main() { }
```
GREASE's heuristics are currently insufficient to avoid the double-free, so
GREASE will output something like the following:
```
clang test.c
grease a.out --symbol test
```
```
Finished analyzing 'test'. Possible bug: double free at segment1+0x116a
```
This result may be unsatisfying if you know that `test` will never be called with `*p
== 0`. By specifying `--initial-precondition`, you can show GREASE how to avoid
the error and continue its analysis. Assuming your compiler targets x86_64
Linux (SysV ABI), write the following in `shapes.txt`:
```
rdi: 000000+0000000000000000

000000: 01 XX XX XX
```
This ensures that loading from `p` will return a symbolic bitvector with the
lowest byte set to `01`. In particular, it will never be zero. Re-running GREASE
yields:
```
grease a.out --symbol test --initial-precondition shapes.txt
```
```
All goals passed!
```

### Conventions

The general-purpose and floating-point registers are named according to the
following schemes:

- AArch32: `R0`, `R1`, `V0`, ..., `V31`
- PPC: `r0`, ..., `r31`, `f0`, ..., `f63`
- x86_64: `rax`, ..., `r15`, `zmm0`, ..., `zmm15`

For LLVM, the arguments are numbered starting from `0` and prefixed with `%`, so
`%0`, `%1`, ..., etc.

[^minimal]: Specifically, all pointer-sized bitvector-typed registers are set to pointers that don't point to anything, and all other bitvector-typed registers are set to symbolic bitvectors.

<!-- Copyright (c) Galois, Inc. 2024. -->

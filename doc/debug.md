# Debugging

This is a guide to debugging the results of GREASE and Screach.

It is meant to be suitable for consumption by both human and coding agents.

## Hangs and timeouts

### Symbolic reads and writes

The GREASE/Screach [memory model](./memory-model.md) can struggle with
unconstrained symbolic reads. Try running with `-v` to check which instructions
are being executed. If you see GREASE/Screach hanging on a load or store, e.g.,

```
Executing instruction at 0x10a9f972: sub    rcx,QWORD PTR [r14+0x8]
```

then the pointer might be symbolic/unconstrained.

To diagnose the problem, you can try using `--debug{,-cmd}` with `break`/`step
N`/`mreg` to inspect the values of the registers at that program point.

To fix the issue, you can try:

- Passing `--arg-buf-init`, `--arg-u64`, or similar flags to specify initial
  values for registers
- Creating an [`--initial-precondition`](./shape-dsl.md) to partially specify
  the values of registers and the contents of memory
- Creating a [startup override](./overrides.md) to partially specify the values
  of register and the contents of memory
- Running from a caller of the function under analysis, which may set the
  registers/memory to values that are more constrained than GREASE's default
  initial precondition

### Exploring unrelated code

By default, GREASE and Screach execute each function that is called. You may
find that symbolic execution enters, explores, and perhaps even errors out in
code that is along or adjacent to paths of interest, but is not itself the focus
of your analysis.

To diagnose the problem, you can try:

- Running with `-v` to see executed function calls and instructions
- Using `--debug{,-cmd}` with `break`/`step N`/`bt` to inspect the call stack
  and see if you're deep into uninteresting code

To fix the issue, you can try:

- Passing `--skip` to skip calls to irrelevant functions
- Using `--arg-*`, `--initial-precondition`, etc. to avoid taking conditional
  branches that explore those paths
- Creating a simplified model (i.e., an [override](./overrides.md)) of the
  irrelevant code
- Passing `--avoid` to Screach

## Errors

### Unsupported instructions

GREASE or Screach might bail on unrecognized instructions:

```
Goal failed:
      0x7db711ab138a: error: in 0x7db711ab1380
      0x7db711ab138f: Could not decode instruction c4e268f5
  <no details available>
```

These are especially common for vectorized instructions, often in libc code
(e.g., for `memcpy`/`memmove` or similar).

To fix the issue, you can try all of the same steps in ["Exploring unrelated
code"](#exploring-unrelated-code) above.

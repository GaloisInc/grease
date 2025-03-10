# Requirements

GREASE features a [memory model](memory-model.md) that is capable of detecting
many types of common software-related issues, such as double-`free`s or
uninitialized stack reads. In addition to the checks that the memory model
performs, GREASE users can opt into checking additional _requirements_ that
assert properties that are not covered by the memory model. The difference
between a memory model check and a requirement is mostly a matter of
performance considerations, as most properties require instrumenting GREASE's
control flow to insert re-run simulation with extra assertions enabled.

To opt into checking a requirement, pass `--req <NAME>` one or more times at
the command line, where `<NAME>` is one of the following:

* `in-text`: Assert that code is never executed from memory that has write
  permissions. In practice, this amounts to checking that the program counter
  only contains addresses from certain parts of the binary, such as the @.text@
  section or PLT stub addresses.

* `no-mprotect`: The requirement that code is not self-modifying. In practice,
  this amounts to checking that a PLT stub named `mprotect` is never called. A
  limitation of this approach is that it can only detect calls to `mprotect`
  from an external library such as `libc`. If `mprotect` is statically linked
  into the binary, for instance, then this requirement would not detect it.

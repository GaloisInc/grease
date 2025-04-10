# Undefined behavior

GREASE aims to find or verify the absence of *bugs* in the program under analysis.
It supports checking both for specific [requirements](requirements.md) and for generally suspicious behaviors.
This page describes the latter.

What constitutes a bug?
When analyzing C, C++, or Rust code, a conservative answer is that any form of reachable undefined behavior in a complete program constitutes a bug.
For machine code, things aren't so clear.

Many behaviors that might be considered undesirable or undefined at the source level have well-defined semantics at the machine code level.
For example, consider a C program with a stack buffer overflow into an adjacent variable.
In the C abstract machine, such behavior is clearly undefined.
However, if the program is compiled in a straightforward way into machine code, the distinction between different stack variables is lost.
The resulting code simply performs several innocuous-looking writes to the stack.
Similarly, adding two signed integers in C might result in an undefined signed overflow.
At the machine code level, there is no distinction betwen signed and unsigned integers, and arithmetic operations generally have a well-defined semantics for all inputs (e.g., based on a two's complement representation).

GREASE aims to be pragmatic in what it calls a bug.
It uses a C-like memory model, and reports behaviors of binaries that would likely constitute undefined behavior in C.
However, as described above, many sources of undefined behavior are impossible or infeasible to detect at the level of machine code.
The following sections detail what behaviors GREASE can and cannot check.

## C11 undefined behaviors

This section describes which forms of undefined behaviors listed in the C11 standard GREASE checks for.
Because the C standard does not offer a comprehensive numbering scheme, we adopt that of the back matter of the [SEI CERT C Coding Standard](https://wiki.sei.cmu.edu/confluence/display/c/CC.+Undefined+Behavior).

The following determinations are quite nuanced.
Please reach out to grease@galois.com if you have further questions about the properties checked by GREASE.

Many undefined behaviors refer to the lexical and syntactic structure of the source C program.
Such behaviors are clearly out of scope for GREASE.
This includes at least behaviors 2, 3, 4, 6, 7, 8, 14, 15, 26-31, 33, 34, 58, 59, and 90-96.

Additional behaviors refer to syntactic categories such as lvalues that do not have corollaries at the binary level.
This includes at least behaviors 12, 13, 18-21, 36, 46, 61, 62.

Further behaviors refer to types that are not available or semantically meaningful at the binary level, such as `void`. This includes at least behavior 22.

The following table lists the remaining behaviors. It is far from complete, and we hope to expand it over time.

<!-- TODO(#85): The remaining rows -->
<!-- TODO(#85): Investigate "?"s -->

| Number  | Description | Checked by GREASE | Notes      |
| ------- | ----------- | ----------------- | ---------- |
| 5 | The execution of a program contains a data race (5.1.2.5). | No | See [limitations](limitations.md) |
| 9 | An object is referred to outside of its lifetime (6.2.4). | Yes | See [Memory model](memory-model.md) |
| 10 | The value of a pointer to an object whose lifetime has ended is used (6.2.4). | Yes | See [Memory model](memory-model.md) |
| 11 | The value of an object with automatic storage duration is used while the object has an indeterminate representation (6.2.4, 6.7.11, 6.8). | Yes | See [Memory model](memory-model.md) |
| 16 | Conversion to or from an integer type produces a value outside the range that can be represented (6.3.1.4). | No | Types are not available at the binary level and all conversions are well-defined |
| 17 | Demotion of one real floating type to another produces a value outside the range that can be represented (6.3.1.5). | ? | Needs test case |
| 23 | Conversion of a pointer to an integer type produces a value outside the range that can be represented (6.3.2.3). | Somewhat | Conversions themselves are not checked, as types are not available. Some forms of pointer arithmetic raise errors. |
| 24 | Conversion between two pointer types produces a result that is incorrectly aligned (6.3.2.3). | ? | Needs test: [#88](https://github.com/GaloisInc/grease/issues/88) |
| 25 | A pointer is used to call a function whose type is not compatible with the referenced type (6.3.2.3). | No | Types are not available at the binary level. |
| 32 | The program attempts to modify a string literal (6.4.5). | Yes | Such literals end up in read-only memory in the binary. |
| 34 | A side effect on a scalar object is unsequenced relative to either a different side effect on the same scalar object or a value computation using the value of the same scalar object (6.5.1). | ? | |
| 35 | An exceptional condition occurs during the evaluation of an expression (6.5.1). | ? | |
| 37 | A function is defined with a type that is not compatible with the type (of the expression) pointed to by the expression that denotes the called function (6.5.3.3). | No | Types are not available at the binary level. |
| 38 | A member of an atomic structure or union is accessed (6.5.3.4).	 | ? | |

## Exceptions

Certain operations generate traps or exceptions at the processor level. GREASE attempts to treat these as errors. These include:

- Division by zero

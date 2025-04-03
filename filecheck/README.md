# FileCheck

## Overview

FileCheck is a Haskell library inspired by the [command-line tool of the same
name][llvm]. It is used to test programs that output text. The testing paradigm
essentially combines golden testing with `grep`.

[llvm]: https://llvm.org/docs/CommandGuide/FileCheck.html

The inputs to FileCheck are the output of the program under test and a sequence
of *commands*. The commands are matched one-by-one against the output; the test
passes if they all match.

The simplest command is `CHECK: <str>`, which checks that the output contains
`<str>`. For example, the following test would pass:

Program output:
```
Hello, world!
```
Commands:
```
CHECK: Hello
CHECK: world
```

If the input to the program under test is a file format that supports comments,
it is often convenient to embed FileCheck commands in the input itself. For
example, the following file could be used to test the `echo` command of a shell:

```sh
# CHECK: Hello, world!
echo "Hello, world!"
```

Reference documentation is a work in progress. For now, see the upstream docs.
Also, FileCheck is used to test itself. See the test suite for usage examples.

## Motivation

In comparison to golden testing, FileCheck-style tests are *coarser*. They only
check particular parts of the program's output. This can cause less churn in the
test-suite when the program output changes in ways that are not relevant to the
properties being tested. However, it is more complex. For example, it requires
learning the FileCheck command language. It can also cause unexpected successes,
e.g., if the program output contains the pattern being checked, but not in the
proper place.

Why build a Haskell library when LLVM already provides their FileCheck tool?
There are a variety of reasons:

1. Ease of adoption: external runtime test dependencies are painful
2. Speed: use as a library avoids file I/O, spawning shells, etc.
3. Flexibility: FileCheck can be used on tools without command-line interfaces

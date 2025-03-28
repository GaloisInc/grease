# FileCheck

FileCheck is a Haskell library inspired by the [command-line tool of the same
name][llvm]. It is used to test programs that output text. The testing paradigm
essentially combines golden testing with `grep`.

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

[llvm]: https://llvm.org/docs/CommandGuide/FileCheck.html

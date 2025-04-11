# FileCheck

## Overview

FileCheck is a Haskell library inspired by the [command-line tool of the same
name][llvm]. It is used to test programs that output text. The testing paradigm
essentially combines golden testing with `grep`.

[llvm]: https://llvm.org/docs/CommandGuide/FileCheck.html

More precisely, FileCheck allows you to build parsers for the output of your
program. The inputs to FileCheck are the output of the program under test and a
separate, generally quite short program written in the FileCheck parsing DSL.

The simplest DSL procedure is `check`, which checks that the output contains a
string. For example, the following test would pass:

Program output:
```
Hello, world!
```
DSL program:
```
check "Hello"
check "world"
```

## Example

Let's say that you have decided to write the first ever Haskell implementation
of a POSIX shell, `hsh`. Here's how to test it with FileCheck.

If the input to the program under test is a file format that supports comments,
it is often convenient to embed FileCheck DSL programs in the input itself. For
example, here's a test for `echo`:

```sh
# check "Hello, world!"
echo 'Hello, world!'
```

Using this strategy, each test case is a single file. You can use the
`directory` package to discover tests from a directory, and `tasty{,-hunit}` to
run tests:

```haskell
module Main (main) where

import Control.Monad (filterM, forM)
import ByteString qualified as BS
import FileCheck qualified as FC
import System.Directory qualified as Dir
import Test.Tasty.HUnit qualified as TTH
import Test.Tasty qualified as TT

-- Code under test:
-- runScript :: ByteString -> (ByteString, ByteString)
-- Returns (stdout, stderr)
import Hsh (runScript)

test :: FilePath -> IO ()
test sh = do
  content <- BS.readFile sh
  (stdout, _stderr) <- runScript content
  let comment = "# "  -- shell script start-of-line comment
  let prog = FC.fromLineComments sh comment content
  result <- FC.check prog (FC.Output stdout)
  TTH.assertBool (sh ++ " contained some assertions") (not (FC.resultNull result))

main :: IO ()
main = do
  let dir = "test-data/"
  entries <- map (dir </>) <$> Dir.listDirectory dir
  files <- filterM Dir.doesFileExist entries
  let shs = List.filter ((== ".sh") . FilePath.takeExtension) files
  let mkTest path = TTH.testCase path (test path)
  let tests = map mkTest shs
  TT.defaultMain (TT.testGroup "hsh tests" tests)
```

Now you can just toss `.sh` scripts into `test-data/` and have them picked up
as tests.

What if you wanted to test both stdout and stderr? You can use two different
styles of comments:
```haskell
test :: FilePath -> IO ()
test sh = do
  -- snip --
  let stdoutComment = "# STDOUT: "
  let prog = FC.fromLineComments sh stdoutComment content
  stdoutResult <- FC.check prog (FC.Output stdout)

  let stderrComment = "# STDERR: "
  let prog' = FC.fromLineComments sh stderrComment content
  stderrResult <- FC.check prog' (FC.Output stderr)

  TTH.assertBool
    (sh ++ " contained some assertions")
    (not (FC.resultNull stdoutResult && FC.resultNull stderrResult))
```

Test cases would then look like so:

```sh
# STDOUT: check "Hello, stdout!"
echo 'Hello, stdout!'
# STDERR: check "Hello, stderr!"
echo 'Hello, stderr!' 1>&2
```

## Cookbook

This section demonstrates how to accomplish common tasks with the FileCheck DSL.
The FileCheck DSL is just [Lua][lua], extended with an API for easy parsing.

[Lua]: https://www.lua.org/ 

Writing tests in Lua offers considerable flexibility and expressive power.
However, with great power comes with great responsibility. Tests should be
as simple as possible, and some repetition should be accepted for the sake of
readability. It is often appropriate to just make a sequence of API calls with
literal strings as arguments.

FileCheck is used to test itself. See the test suite for additional examples.

### Long matches

Match large blocks of text with Lua's multi-line string syntax:
```
some
multi-line
    text
```
```lua
check [[
some
multi-line
    text
]]
```

### Repetition

Match repetitive text using a variable:

```
HAL: Affirmative, Dave. I read you.
Dave: Open the pod bay doors, HAL.
HAL: I'm sorry, Dave. I'm afraid I can't do that.
```
```lua
name="Dave"
check("Affirmative, " .. name)
check("I'm sorry, " .. name)
```
or with a `for`-loop:
```
Step 1: Learn about FileCheck
Step 2: Use it to test your project
Step 3: Enjoy!
```
```lua
for i=1,3 do
  check("Step %d".format(i))
end
```

### Checking for generated text

To check that some dynamically-generated text appears elsewhere in the output,
use the `string` library and the `text` variable:
```
Generating a random number...
Generated 37106428!
Printing 37106428...
```
```lua
check "Generated "
num=string.find(text, "^%d+")
check(string.format("Printing %d...", num))
```

The above example borders on *too much* logic for a test-case. Use discretion
when writing tests!

## API reference

The Lua API is *stateful*. It keeps track of a global variable `text` that
is initialized to the output of the program under test. Various API functions
cause the API to seek forward in `text`. This is analogous to working file-like
objects in languages like C or Python. `text` should not be updated from Lua
code; such updates will be ignored by FileCheck.

### High-level API

- `check(s: String)`: Find `s` in `text`. Seek to after the end of `s`. Like
  LLVM FileCheck's `CHECK`.
- `here(s: String)`: Check that `text` beings with `s`. Seek to after the end
  of `s`.
- `line(String)`: `line(s)` is equivalent to `here(s .. "\n")`.

### Low-level API

- `fail()`: Fail to match at this point in `text`.
- `match(n: Integer)`: Consume `n` bytes of `text`, treating them as a match.
- `seek(n: Integer)`: Seek forward `n` bytes in `text`.

## Motivation

The overall FileCheck paradigm is a form of [data driven testing]. See that blog
post for considerable motivation and discussion.

[data driven testing]: https://matklad.github.io/2021/05/31/how-to-test.html#Data-Driven-Testing

In comparison to golden testing, FileCheck-style tests are *coarser*. They only
check particular parts of the program's output. This can cause less churn in
the test-suite when the program output changes in ways that are not relevant
to the properties being tested. The fineness of golden testing can force
devlopers to adopt [complex] [workarounds], these can sometimes be obviated by
FileCheck-style testing.

[complex]: https://rustc-dev-guide.rust-lang.org/tests/ui.html#normalization 
[workarounds]: https://github.com/GaloisInc/crucible/blob/dc0895f4435dc19a8cceee3272c9a508221bce51/crux-llvm/test/Test.hs#L226-L311

However, it is more complex. For example, it requires learning the FileCheck
DSL. It can also cause unexpected successes, e.g., if the program output
contains the pattern being checked, but not in the proper place.

Why build a Haskell library when LLVM already provides their FileCheck tool?
There are a variety of reasons:

1. Ease of adoption: external runtime test dependencies are painful
2. Speed: use as a library avoids file I/O, spawning shells, etc.
3. Flexibility: FileCheck can be used on tools without command-line interfaces

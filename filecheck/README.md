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

## Example

Let's say that you have decided to write the first ever Haskell implementation
of a POSIX shell, `hsh`. Here's how to test it with FileCheck.

If the input to the program under test is a file format that supports comments,
it is often convenient to embed FileCheck commands in the input itself. For
example, here's a test for `echo`:

```sh
# CHECK: Hello, world!
echo 'Hello, world!'
```

Using this strategy, each test case is a single file. You can use the
`directory` package to discover tests from a directory, and `tasty{,-hunit}` to
run tests:

```haskell
module Main (main) where

import Control.Monad (filterM, forM)
import Data.Text.IO qualified as Text.IO
import FileCheck qualified as FC
import System.Directory qualified as Dir
import Test.Tasty.HUnit qualified as TTH
import Test.Tasty qualified as TT

-- Code under test:
-- runScript :: Text -> (Text, Text)
-- Returns (stdout, stderr)
import Hsh (runScript)

test :: FilePath -> IO ()
test sh = do
  let prefix = Nothing
  let comment = "# "  -- shell script start-of-line comment
  content <- Text.IO.readFile sh
  (stdout, _stderr) <- runScript content
  (cmds, _result) <- FC.parseCommentsAndCheck' prefix comment (Just sh) content (FC.Output stdout)
  TTH.assertBool (sh ++ " contained some assertions") (not (null cmds))

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

What if you wanted to test both stdout and stderr? You can use a `Prefix`:
```haskell
test :: FilePath -> IO ()
test sh = do
  -- snip --
  let stdoutPfx = Just (FC.Prefix "STDOUT")
  (stdoutCmds, _result) <-
    FC.parseCommentsAndCheck' stdoutPfx comment (Just sh) content (FC.Output stdout)

  let stderrPfx = Just (FC.Prefix "STDERR")
  (stderrCmds, _result) <-
    FC.parseCommentsAndCheck' stderrPfx comment (Just sh) content (FC.Output stderr)

  TTH.assertBool
    (sh ++ " contained some assertions")
    (not (null (stdoutCmds ++ stderrCmds)))
```

Test cases would then look like so:

```sh
# STDOUT: Hello, stdout!
echo 'Hello, stdout!'
# STDERR: Hello, stderr!
echo 'Hello, stderr!' 1>&2
```

## Documentation

Reference documentation is a work in progress. For now, see the upstream docs.
Also, FileCheck is used to test itself. See the test suite for usage examples.

## Motivation

The overall FileCheck paradigm is a form of [data driven testing]. See that blog
post for considerable motivation and discussion.

[data driven testing]: https://matklad.github.io/2021/05/31/how-to-test.html#Data-Driven-Testing

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

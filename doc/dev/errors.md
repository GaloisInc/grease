# Error handling

The error handling policy in GREASE supports the following goals:

- Modularity: Each module of GREASE should be fairly independent of the others.
- Robustness: GREASE should be reasonably robust to unexpected inputs and
  circumstances.
- User-friendliness: When something goes wrong, GREASE should provide some
  helpful information to the user (or developer).

This document describes our general approach, but we trust one another to make
exceptions to these rules as appropriate.

## The ideal

The following checklist encodes our preferred error-handling strategy:

- [ ] Every fallible operation has its own error type
- [ ] The error type implements `Pretty`
- [ ] The `Pretty` instance provides substantial information on what went wrong
  - [ ] It may include pretty-printed representations of the data in question
  - [ ] It describes why that data is unworkable
  - [ ] It may provide hints about how the situation might be fixed
- [ ] The constructors of the error type are exported
- [ ] The function returns some variant of `Either`
- [ ] `panic` should be used in only and all situations that are expected to be
  impossible to trigger using the exported API

For example:
```haskell
module Foo (
  Baz,
  BarError(..),
  bar,
) where

import Grease.Panic (panic)
import Prettyprinter qualified as PP

-- | Error type for 'bar'
data BarError
  = BadBaz Baz
  | UnexpectedQuux

instance PP.Pretty BarError where
  pretty =
    \case
      BadBaz baz ->
        PP.unlines
        [ "The module contained a bad baz:"
        , PP.pretty baz
        , "Expected a baz with..."
        , "Hint: try ..."
        ]
      UnexpectedQuux -> "Found quux with..."

bar :: Baz -> IO (Either BarError BarResult)
bar =
  \case
    ImpossibleBaz ->
      panic
        "bar"
        [ "Encountered `ImpossibleBaz`!"
        , "This isn't possible because ..."
        ]
    OkBaz -> undefined -- ...
```

## Handling errors

In `lib:grease`, errors are generally propagated up to top-level API
entrypoints. In `exe:grease-exe`, they are categorized into a few different
buckets:

- User errors, e.g., an invalid configuration
- Malformed inputs (e.g., LLVM modules or binaries)
- Unsupported features

This categorization helps the user to know what to do next:

- In case of a user error, the user should revise their configuration.
- In case of malformed inputs, the user should inspect the output of the
  compiler or other tool.
- In the case of an unsupported feature, the user should file a bug.

This categorization essentially has to happen at the application level. Without
full context, it is often not clear which errors are user errors vs. programmer
errors.

## No `Exception`s

With a few exceptions, we disallow the use of `Exception`s in GREASE.

- Exceptions are not reflected in type signatures, making it harder for
  developers to notice or recall when code can throw an exception. This
  makes it less likely for them to be handled properly.
- Exceptions tend to lack *context*. They can be thrown very deep in a
  call stack, only to be reported at the very top level.
- Exceptions can only be caught in effectful (`IO`) code, making them less
  flexible.

We allow:

- `IOException`s, which GREASE is generally not expected to be robust against.
- Panics, which are unrecoverable.

We also encourage the use of `Control.Exception.assert`, which will be checked
when running tests but disabled otherwise (TODO(#435)).

GREASE installs a top-level exception handler that catches any uncaught
exceptions and asks users to file bugs about them.

## Partiality

Our policy on exceptions precludes the use of partial functions such as `error`,
`fromJust`, and `undefined`. In situations where the developer is tempted to
reach for such functions, they should instead use an explicit error type if the
exported API doesn't make the error unreachable, or explicitly use `panic` with
a message explaining why the error is in fact unreachable.

# Style

This document describes a few aspects of the coding style in use in GREASE. The
main idea is to document expectations that frequently arise as comments during
code review. We strive to document our consensus, rather than enforce opinions.
We trust one another to make exceptions to these rules as appropriate. We
currently do not provide much in the way of justification for these guidelines,
but may do so in the future.

Some of these expectations are codified and enforced in CI using automated
tooling such as GHC warnings, hlint, or ad-hoc scripts.

This is a description of practices we have found useful in the context of
GREASE, not a recommendation or prescription for other projects or contexts.

## Exports

Every module has an explicit [export list].

[export list]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-warnings.html#ghc-flag-Wmissing-export-lists

## Haddocks

Exported functions and types are documented with [Haddock].

[Haddock]: https://haskell-haddock.readthedocs.io/latest/

## Imports

Imports are qualified or have explicit import lists. Qualified imports are
written [in postpositive position]. Imports are in one large block that is
alphabetically sorted.

[in postpositive position]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/import_qualified_post.html

## Naming

Functions and variable names are written in `camelCase`. Type, class, and module
names are written in `UpperCamelCase`.

## Newtypes

`newtype`'s are used frequently. [This post][newtype] has a discussion of the
practice in Rust, which applies *mutatis mutandis* to Haskell.

[newtype]: https://rust-unofficial.github.io/patterns/patterns/behavioural/newtype.html

## Numbered `TODO`s

`TODO` comments are used liberally, and are formatted `TODO(#NNN)` where `#NNN`
is a GitHub issue number.

## Superfluous syntax

We avoid superfluous syntax. For example, we avoid writing `f $ x` or `f (x)`
when just `f x` would do. A common case is a superfluous `do`. Instead of
```haskell
f :: a -> IO b
f x = do
  g x
```
Write:
```haskell
f :: a -> IO b
f x = g x
```

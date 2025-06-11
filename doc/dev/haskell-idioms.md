# Fancy Haskell idioms

This page lists a few idioms that are used in GREASE and might be a bit
surprising when you first see them. To understand these idioms, you should be
comfortable with GADTs, kinds, and monads.

## Types with names that start with `Some`

Haskell currently lacks support for [first-class existential types]. They can be
emulated with [GADT]s like so:
```haskell
{-# LANGUAGE GADTs #-}

data Some f where
  Some :: f x -> Some f  -- the return type does not mention `x`
```
(`Some` is canonically available as [`Data.Parameterized.Some.Some`].) As
defined above, `Some` has kind `forall k. (k -> Type) -> Type`. What if you want
to existentially quantify over something of a different kind, e.g., you want to
close over the first parameter of a datatype with two parameters? Then, you have
to make your own GADT, and such types are conventionally named `Some*`.

[first-class existential types]: https://github.com/ghc-proposals/ghc-proposals/pull/473

## Faux `let`-bindings in type signatures

Sometimes, a long type like `AVeryLongTypeName a (AnotherLongName b)` appears
several times in one signature:
```haskell
foo ::
  ( Eq (AVeryLongTypeName a (AnotherLongName b))
  , Hashable (AVeryLongTypeName a (AnotherLongName b))
  , Show (AVeryLongTypeName a (AnotherLongName b))
  ) =>
  AVeryLongTypeName a (AnotherLongName b) ->
  AVeryLongTypeName a (AnotherLongName b) ->
  ()
foo _x _y = ()
```
One could define a type synonym
```haskell
type ShorterName a b = AVeryLongTypeName a (AnotherLongName b)
```
However, this forces readers of the code to know about (and remember) the
synonym. Alternatively, one can use equality constraints to form a "faux
`let`-binding" like so:
```haskell
foo ::
  ( t ~ AVeryLongTypeName a (AnotherLongName b)
  , Eq t
  , Hashable t
  , Show t
  ) =>
  t ->
  t ->
  ()
foo _x _y = ()
```

## Pattern matching on `pure` values in `do` blocks

[GADT] constructors can existentially quantify types.
```haskell
{-# LANGUAGE GADTs #-}

data Some f where
  Some :: f x -> Some f  -- the return type does not mention `x`
```
(`Some` is canonically available as [`Data.Parameterized.Some.Some`].) To use
a `Some` value, you generally pattern match on it. This brings the captured
variable into scope (though just as an unconstrained variable, so any code you
write after the pattern match has to be polymorphic over it).
```haskell
someLength :: Some [] -> Int
someLength someList =
  case someMaybe of
    -- Can use l here, but have to be completely polymorphic over the element
    -- type. We can use `length`, because it is polymorphic enough (it has type
    -- `forall a. [a] -> Int`).
    Some l -> length l

main :: IO ()
main = putStrLn (wasItJust (Some (Just "ok")))
```
Sometimes, we want to bring the variable into scope by pattern-matching on
the constructor, but we'd rather not use a `case` statement, as it increases
indentation (e.g., we might be pattern-matching on several `Some`s in a row).
We can't do this with `let`, because the type variable is only in scope for the
right-hand side of the `=`.
```haskell
makeSome :: Int -> Some Maybe
makeSome = _ -- ...

wontWork :: IO ()
wontWork = do
  let Some x = makeSome 4
  -- try something with x...
  pure ()
```
Not only will this not compile, it'll give us a confusing error message:
```
Couldn't match expected type ‘p0’ with actual type ‘Maybe x’
        because type variable ‘x’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a pattern with constructor:
        ...
```
However, this *is* possible using `do`-notation. Since our `Some` is just a
pure value (not a monadic action), we have to "inject" it into the monad using
`pure`:
```haskell
printWasItJust :: IO ()
printWasItJust = do
  Some x <- pure (makeSome 5)
  -- Again, can use x here, but only polymorphically
  when (isJust x) $
    putStrLn "it was Just"
```
To understand why this works, desugar the `do` notation:
```haskell
printWasItJust :: IO ()
printWasItJust =
  pure (makeSome 5) >>= \(Some x) -> when (isJust x) (putStrLn "it was Just")
```
That's why you might see `do pat <- pure val`.

[`Data.Parameterized.Some.Some`]: https://hackage-content.haskell.org/package/parameterized-utils-2.1.10.0/docs/Data-Parameterized-Some.html#t:Some

## Matching on constraint-capturing constructors in `do` blocks

[GADT] constructors can "capture" constraints, which can be recovered by
pattern-matching on them. Note the lack of a `Show a` constraint on `showIt` in
the following example:
```haskell
{-# LANGUAGE GADTs #-}

data Showable a where
  Showable :: Show a => a -> Showable a

showIt :: Showable a -> String
showIt (Showable x) = show x  -- pattern matching brings Show a into scope

main :: IO ()
main = putStrLn (showIt (Showable "hello"))
```
(Operationally, the GADT constructor holds the dictionary for
the class, which is like a C++ vtable, see ["Internals of Type
Classes"](https://nikivazou.github.io/CMSC498V/lectures/TypeClasses.html)
for more information.) GHC's built-in type equality constraint `~` is often
"captured" this way, e.g.,
```haskell
{-# LANGUAGE GADTs #-}

data IsString a where
  IsString :: a ~ String => IsString a

isStr :: IsString a -> a -> String
isStr is v =
  case is of
    IsString -> v  -- can't return `v` without pattern-matching on `is`

main :: IO ()
main = putStrLn (isStr IsString "hello")
```
The canonical constructor for capturing equality constraints is called
[`Data.Type.Equality.Refl`]. Note that the above definition of `IsString` is
equivalent to writing
```haskell
data IsString a where
  IsString :: IsString String
```
which more analogous to how `Refl` is defined.

Sometimes, we want to bring the constraint into scope by pattern-matching on
the constructor, but we'd rather not use a `case` statement, as it increases
indentation (e.g., we might be pattern-matching on several `Refl`s in a row).
This is possible using `do`-notation and pattern matching on a `pure` value,
just like with `Some`:
```haskell
{-# LANGUAGE GADTs #-}

import Data.Type.Equality ((:~:)(Refl))

type IsString a = a :~: String

printStr :: IsString a -> a -> IO ()
printStr is v = do
  Refl <- pure is
  putStrLn v

main :: IO ()
main = printStr Refl "hello"
```

[`Data.Type.Equality.Refl`]: https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Type-Equality.html#t::-126-:
[GADT]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt.html

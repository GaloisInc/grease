{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Cli.Enum (
  -- * Low-level helpers
  enumMap,
  parseBounded,

  -- * High-level helpers
  enumParser,
  enumParserDefault,
) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Options.Applicative qualified as Opt

camelCaseToKebabCase :: String -> String
camelCaseToKebabCase = foldl go "" . lowerFirst
 where
  lowerFirst =
    \case
      [] -> []
      (c : cs) -> Char.toLower c : cs
  go acc c =
    acc ++ if Char.isUpperCase c then "-" ++ [Char.toLower c] else [c]

-- Given an enumeration type, construct an @optparse-applicative@ metavar that
-- displays all possible variants. For example, given @data Letter = A | B | C@,
-- this would produce the metavar @(A|B|C)@.
boundedEnumMetavar ::
  forall a f proxy.
  (Bounded a, Enum a, Opt.HasMetavar f, Show a) =>
  proxy a ->
  Opt.Mod f a
boundedEnumMetavar _ = Opt.metavar $ varShowS ""
 where
  -- Use ShowS (i.e., a difference list of strings) below to amortize the cost
  -- of appending strings.
  varShowS :: ShowS
  varShowS =
    showParen True $
      List.foldr (.) id $
        List.intersperse (showChar '|') $
          List.map
            ((camelCaseToKebabCase .) . shows)
            [minBound @a .. maxBound @a]

-- | Create a 'Map' of 'Enum' values with kebab-case names as keys.
enumMap :: (Bounded a, Enum a, Show a) => Map String a
enumMap = Map.fromList [(name v, v) | v <- [minBound .. maxBound]]
 where
  name = camelCaseToKebabCase . show

-- | Parse a 'Bounded' 'Enum' by converting 'show'n values to kebab case.
parseBounded :: (Bounded a, Enum a, Show a) => String -> Maybe a
parseBounded s = Map.lookup s enumMap

-- | Parse a 'Bounded' 'Enum' from kebab-case.
enumParser ::
  forall a.
  (Bounded a, Enum a, Show a) =>
  [Opt.Mod Opt.OptionFields a] ->
  Opt.Parser a
enumParser o =
  Opt.option (Opt.maybeReader parseBounded) $
    mconcat
      [ mconcat o
      , boundedEnumMetavar (Proxy @a)
      , Opt.completeWith (Map.keys (enumMap @a))
      ]

-- | Parse a 'Bounded' 'Enum' from kebab-case, with a default value.
enumParserDefault ::
  forall a.
  (Bounded a, Enum a, Show a) =>
  a ->
  [Opt.Mod Opt.OptionFields a] ->
  Opt.Parser a
enumParserDefault a o =
  enumParser (o ++ [Opt.showDefault, Opt.value a])

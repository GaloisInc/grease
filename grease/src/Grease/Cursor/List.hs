{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Cursor.List (
  Last,
  Snoc,
  lastCons,
  lastSnoc,
) where

import Data.Parameterized.Axiom (unsafeAxiom)
import Data.Parameterized.List (List (..))
import Data.Type.Equality ((:~:) (Refl))
import Grease.Panic (panic)

type family Last (ts :: [k]) :: k where
  Last '[t] = t
  Last (_ ': ts) = Last ts

type family Snoc (as :: [k]) (b :: k) where
  Snoc '[] b = '[b]
  Snoc (a ': as) b = (a ': Snoc as b)

-- | If a list has a last item (i.e., is non-empty), then that last item
-- doesn't change when you add something to the front.
lastCons :: Last ts ~ s => proxy1 t -> proxy2 ts -> Last (t ': ts) :~: s
lastCons _t _ts = unsafeAxiom

-- | After 'Snoc'-ing something to the end of a list, it's the last element.
lastSnoc :: proxy1 t -> proxy2 ts -> Last (Snoc ts t) :~: t
lastSnoc _t _ts = unsafeAxiom

-- Below is some justifiction for the above axioms. Not meant to be exported.

notLast :: Last '[] ~ t => a
notLast =
  panic
    "notLast"
    [ "Impossible: From the definition of `Last`"
    , "There is no pattern match on the empty list."
    ]

-- Termination: Structurally shrinking second parameter
_lastCons :: Last ts ~ s => proxy t -> List proxy ts -> Last (t ': ts) :~: s
_lastCons _proxy =
  \case
    Nil -> notLast
    t :< rest ->
      case _lastCons t rest of
        Refl -> Refl

-- Termination: Structurally shrinking second parameter
snocRepr :: proxy t -> List proxy ts -> List proxy (Snoc ts t)
snocRepr proxy =
  \case
    Nil -> proxy :< Nil
    t :< rest -> t :< snocRepr proxy rest

-- Termination: Structurally shrinking second parameter
_lastSnoc :: proxy t -> List proxy ts -> Last (Snoc ts t) :~: t
_lastSnoc proxy =
  \case
    Nil -> Refl
    t :< rest ->
      case _lastCons t (snocRepr proxy rest) of
        Refl ->
          case _lastSnoc proxy rest of
            Refl -> Refl

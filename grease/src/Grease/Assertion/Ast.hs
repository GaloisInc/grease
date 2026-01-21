{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Grease.Assertion.Ast (annotatedExpr, SepAssert (..), Lit (..), Expr (..), BaseExpr (..)) where

import Control.Lens (makeLenses, (&))
import Control.Lens.Getter ((^.))
import Control.Lens.Operators ((.~))
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.BitVector.Sized (BV)
import Data.Map qualified as Map
import Data.Parameterized qualified as Param
import Text.LLVM (unreachable)

{-
lit :=
    | 0xA:width
    | true
    | false

type :=
    | Bool
    | Ptr
    | Bitvector(nat)

var :=
    | ident // args
    | #ident // existential
    | $ident // abstract label

expr :=
    // annot
    | type_of(expr, type)
    | (expr)
    // Bv casts
    zext(expr, nat)
    sext(expr, nat)
    trunc(expr, nat)
    // BV\v
    | expr < expr
    | expr <= expr
    | expr $< expr
    | expr $<= expr
    // arith rules Bv
    | expr + expr
    | expr - expr
    | expr * expr
    // a' a' -> Bool
    | expr = expr
    // Bool rules
    | !expr
    | ite(expr, expr, expr)
    | expr /\ expr
    | expr \/ expr
    | var
    | lit

sepform :=
    | pure(expr) // note this is parser derived not syntactic
    | is_ptr(expr, expr, expr)
    | uninit_cell(expr, expr, expr) // (label, offset, size)
    | init_cell(expr, expr, expr) (label, offset, mem_val)
    | size(expr, expr)
    | sepform * sepform

Let W = the bit width of pointers for the given arch context
typing:
gamma |- e : Bool
----
gamma |- pure(e) : SepBool

gamma |- e1 : Ptr,  gamma |- e2: Label, gamma |- e3: Bitvector(W)
----
gamma |- is_ptr(e1, e2, e3) : SepBool

gamma |- e1 : Label, gamma |- e2 : Bitvector(W), gamma |- e3 : Bitvector(W)
---
uninit_cell(e1, e2, e3)

gamma |- e1 : Label, gamma |- e2 : Bitvector(W), gamma |- e3 : mem_val
---
init_cell(e1, e2, e3)

gamma |- e1 : Label, gamma |- e2 : Bitvector(W)
---
gamma |- size(e1, e2)

gamma |- s1 : SepBool, gamma |- s2 : SepBool
----
s1 * s2

gamma e1 |- Bitvector(a), gamma e2 |- Bitvector(b), a = b
---- compare (#)
gamma |- e1 # e2 : Bool

gamma |- e1 : t1
---- typeof
gamma |- type_of(e1, t1) : t1

gamma |- e1 : Bool, gamma |- e1: Bool
--- ^ bool binop
gamma |- e1 ^ e2 : Bool

gamma |- e1 : Bool
----
gamma |- !e1 : Bool

gamma |- e1 : Bool, gamma |- e2: A, gamma |- e3: A
---- ite
gamma |- ite(e1, e2, e3): A

x : A in gamma
---
x : A

---
\$l : Label

--- true
true : Bool

--- false
false : Bool

--- bv
bv(_, n) : Bitvector(n)

gamma |- e1 : Ptr
---
gamma |- e1 : mem_val

gamma |- e1 : Bitvector a
---
gamma |- e1 : mem_val

-}

data VarType
  = Existential
  | Argument
  | Label
  deriving (Show, Eq, Ord)

data Var = Var
  { varName :: String
  , varType :: VarType
  }
  deriving (Show, Eq, Ord)

data Lit
  = LitBool Bool
  | forall w. LitBv {width :: Param.NatRepr w, value :: BV w}

data BoolBinOp
  = Or
  | And

data BvBinOp
  = Plus
  | Minus
  | Times

data AssrtType
  = Bool
  | Ptr
  | Bitvector Int
  | LabelTy
  deriving (Show, Eq, Ord)

data BaseExpr t
  = ExprVar Var
  | ExprLit Lit
  | BoolBinOpExpr (BoolBinOp, Expr t, Expr t)
  | BvBinOpExpr (BvBinOp, Expr t, Expr t)
  | TypeOf (Expr t, AssrtType)

data Expr t = Expr {_baseExpr :: BaseExpr t, _annotation :: t}
makeLenses ''Expr
annotatedExpr :: BaseExpr t -> t -> Expr t
annotatedExpr b annot = Expr{_baseExpr = b, _annotation = annot}

data SepAssert t
  = Pure (Expr t)
  | IsPtr (Expr t, Expr t, Expr t)
  | UninitCell (Expr t, Expr t)
  | InitCell (Expr t, Expr t, Expr t)
  | Size (Expr t, Expr t)
  | SepConj (SepAssert t) (SepAssert t)

type TypeState = Map.Map Var AssrtType
type TypeMonad = StateT TypeState (Either String)

infer :: Expr t -> TypeMonad (Expr AssrtType)
infer eAnnot = do
  let base = eAnnot ^. baseExpr
  nbase <- case base of
    BoolBinOpExpr (op, e1, e2) -> do
      e1' <- check e1 Bool
      e2' <- check e2 Bool
      return $ annotatedExpr (BoolBinOpExpr (op, e1', e2')) Bool
    TypeOf (e1, ty) -> do
      e1' <- check e1 ty
      return $ annotatedExpr (TypeOf (e1', ty)) ty
    _ -> lift $ Left "unable to infer type, try to annotate"
  return nbase

check :: Expr t -> AssrtType -> TypeMonad (Expr AssrtType)
check eAnnot aty = do
  let base = eAnnot ^. baseExpr
  case base of
    BvBinOpExpr (op, e1, e2) -> do
      e1' <- check e1 aty
      e2' <- check e2 aty
      return $ annotatedExpr (BvBinOpExpr (op, e1', e2')) aty
    _ -> do
      inferred <- infer eAnnot
      if inferred ^. annotation == aty
        then
          return inferred
        else
          -- FIXME: we should use diagnostic or something to give good type errors
          lift $ Left $ "Expected a type but got another one"

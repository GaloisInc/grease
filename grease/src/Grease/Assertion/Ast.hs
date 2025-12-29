module Grease.Assertion.Ast () where

import Data.BitVector.Sized (BV)
import Data.Parameterized qualified as Param

{-
lit :=
    | bv(hex, nat)
    | true
    | false

tyoe :=
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
    | (ite expr expr expr)
    | expr /\ expr
    | expr \/ expr
    | var
    | lit

sepform :=
    | pure(expr) // note this is parser derived not syntactic
    | is_ptr(expr, expr, expr)
    | uninit_cell(expr, expr, expr) // (label, offset, size)
    | init_cell(expr, expr, expr) (label, offset, memvalue)
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
size(e1, e2)

gamma |- s1 : SepBool, gamma |- s2 : SepBool
----
s1 * s2

gamma e1 |- Bitvector(W)
---- arith (%)
e1 % e2
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

data Expr
  = ExprVar Var
  | ExprLit Lit

data SepAssert
  = Pure Expr
  | IsPtr (Expr, Expr, Expr)
  | UninitCell (Expr, Expr)
  | InitCell (Expr, Expr, Expr)
  | Size (Expr, Expr)
  | SepConj Expr Expr

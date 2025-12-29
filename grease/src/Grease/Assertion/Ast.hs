module Grease.Assertion.Ast () where

import Control.Monad.State (StateT)
import Data.BitVector.Sized (BV)
import Data.Map qualified as Map
import Data.Parameterized qualified as Param

{-
lit :=
    | bv(hex, nat)
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

data BaseExpr
  = ExprVar Var
  | ExprLit Lit

data Expr t = Expr {baseExpr :: BaseExpr, annotation :: T}

data SepAssert
  = Pure Expr
  | IsPtr (Expr, Expr, Expr)
  | UninitCell (Expr, Expr)
  | InitCell (Expr, Expr, Expr)
  | Size (Expr, Expr)
  | SepConj Expr Expr

data AssrtType
  = Bool
  | Ptr
  | Bitvector Int
  | LabelTy
  deriving (Show, Eq, Ord)

type TypeState = Map.Map Var AssrtType
type TypeMonad = StateT TypeState (Either String)

infer :: TypeState -> Expr t -> TypeMonad Expr AssrtType
infer = undefined

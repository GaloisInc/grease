module Grease.Assertion.Ast () where

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
    | type_of(expr, type)
    | (expr)
    | expr < expr
    | expr <= expr
    | expr $< expr
    | expr $<= expr
    | expr /\ expr
    | expr \/ expr
    | expr + expr
    | expr - expr
    | expr * expr
    | !expr
    | (ite expr expr expr)
    | var
    | lit

spepform :=
    | pure expr
    | is_ptr(expr, expr, expr)
    | uninit_cell(expr, expr)
    | init_cell(expr, expr, expr)
    | size(expr, expr)
    | sepform * sepform
-}

data Expr = Expr Int

data SepAssert
  = Pure Expr
  | IsPtr (Expr, Expr, Expr)
  | UninitCell (Expr, Expr)
  | InitCell (Expr, Expr, Expr)
  | Size (Expr, Expr)
  | SepConj Expr Expr

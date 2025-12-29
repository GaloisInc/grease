module Grease.Assertion.Ast () where

{-
lit :=
    | bv(hex, nat)
    | true
    | false
expr :=
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
    | lit

spepform :=
    | pure expr
    | is_ptr(expr, expr, expr)
    | uninit_cell(expr, expr)
    | init_cell(expr, expr, expr)
    | size(expr, expr)
    | sepform * sepform
-}

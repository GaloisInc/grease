module Grease.Assertion.Assrt () where

import Data.Map qualified as Map
import Data.Parameterized qualified as Param
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Nonce qualified as Nonce
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as C
import What4.Expr qualified as W4
import What4.Interface qualified as WI

-- So we want to parse the assertion before we hit the setup, so
-- it needs to be quantified by an arg assignment that cursors will resolve to

-- We essentially want to be able to represent WI Preds and WI.SymExprs with holes
-- where those holes are cursors

-- | We define a type that exposes the type of variable assignment we would expect
-- and from that assignment returns an expression
-- internally we will represent this by closing over an Assignment to symbolic
-- vars that will be bound by equality. We also need to do a similar thing for BlockIds for
-- abstract allocations, those will be bound by a generated binding map

-- So a substitution is really a binding from symexprs to blockids
-- and a binding of symexprs to ArgName
-- newtype ShapeValue = ShapeValue (Ctx.Assignment Int Int)

-- Externally our types are:
-- t :=
--   | Bool
--   | mem_val
--   | Bv w
--   | Label
--   w := Nat
--   | Ptr
-- type rules
-- 'a = 'a => Bool
-- is_ptr(Ptr, Label, Bv w) => Bool -- this forces a binder... if we do this both for destructing and constructing,
-- we could also do this a bit more simply as a formation rule of mem_ptr, but that seems less ideal
-- mem_ptr(Ptr) => mem_val
-- mem_bv(Bv w) => mem_val

data Bindings sym argTys = Bindings
  { blocks :: Map.Map (WI.SymNat sym) (WI.SymNat sym)
  , args :: Ctx.Assignment (CS.RegValue' sym) argTys
  }

data BindableExpr sym tp argTys = BindableExpr
  { bexpr :: WI.SymExpr sym tp
  , bindings :: Bindings sym argTys
  }

-- Unfortunately nat not in Expr due to invariants so separate type
data BindableNat sym = BindableNat
  { symNat :: WI.SymNat sym
  , natBindings :: Map.Map (WI.SymNat sym) (WI.SymNat sym)
  }

-- We only allow pointer introduction and elimination... are there other types we should be representing?

-- | We want to be able to perform substitutions in a pointer so we represent them internally
data BindablePointer sym wptr argTys = BindablePointer
  { pointerBlock :: BindableNat sym
  , pointerOffset :: BindableExpr sym (WI.BaseBVType wptr) argTys
  }

-- current problem we have a bit of a weird situation where to interact with the mem model we have non
-- symbolic stuff we'd like to do. That is we want to form a ptr(x,a) but this can only be a top, level thing
-- and is only consumeable in the store position...
-- imagine if we have a = ptr(l,o)... what does that mean... my view maybe.. is we could in theory do
-- init_cell_ptr() but then this is restricting ptrs, perhaps we need to unify exprs and ptr exprs
-- the trouble is making a bindable regValue is... annoying
data MemVal sym wptr argTys
  = MemPtr (BindablePointer sym wptr argTys)
  | forall w. MemBv {memBvWidth :: Param.NatRepr w, memBvExpr :: BindableExpr sym (WI.BaseBVType w) argTys}

data PtrVar argTys wptr s
  = PtrVarArg (Ctx.Index argTys (CLM.LLVMPointerType wptr))
  | PtrExistential (Nonce.Nonce s ())

data Assertion sym wptr argTys s
  = Pure (BindableExpr sym WI.BaseBoolType argTys)
  | -- a store can be anything at a crucible type... we need to handle pointer types
    -- So we need to be able to bind the RegValue...
    forall tp. InitCell (BindablePointer sym wptr argTys, MemVal sym wptr argTys)
  | UninitCell (BindablePointer sym wptr argTys, BindableExpr sym (WI.BaseBVType wptr) argTys)
  | Size (BindableExpr sym (WI.BaseBVType wptr) argTys) -- TODO: Maybe bounds is preferred for compositional reasoning
  | IsPtr (PtrVar argTys wptr s) -- we either are constructing this against a var or an index of an arg..., this is because the only pointers can
  -- be existentials or prepopulated we cant make a Ptr

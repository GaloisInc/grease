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
  { args :: Ctx.Assignment (CS.RegValue' sym) argTys
  }

-- current problem we have a bit of a weird situation where to interact with the mem model we have non
-- symbolic stuff we'd like to do. That is we want to form a ptr(x,a) but this can only be a top, level thing
-- and is only consumeable in the store position...
-- imagine if we have a = ptr(l,o)... what does that mean... my view maybe.. is we could in theory do
-- init_cell_ptr() but then this is restricting ptrs, perhaps we need to unify exprs and ptr exprs
-- the trouble is making a bindable regValue is... annoying
data MemVal sym wptr argTys s
  = MemPtr (PtrVar argTys wptr s)
  | forall w. MemBv {memBvWidth :: Param.NatRepr w, memBvExpr :: WI.SymBV sym w}

data PtrVar argTys wptr s
  = PtrVarArg (Ctx.Index argTys (CLM.LLVMPointerType wptr))
  | PtrExistential (Nonce.Nonce s ())

newtype LabelNonce s = LabelNonce (Nonce.Nonce s ())

-- | An assertion pointer is represented by an offset and
-- a nonce value that represents equivalence classes between malloced blocks
data AssrtPointer sym wptr s = AssrtPointer {assrtPointer :: LabelNonce s, assrtOffset :: WI.SymBV sym wptr}

data Assertion sym wptr argTys s
  = Pure (WI.Pred sym)
  | -- a store can be anything at a crucible type... we need to handle pointer types
    -- So we need to be able to bind the RegValue...
    forall tp. InitCell (AssrtPointer sym wptr s, MemVal sym wptr argTys s)
  | UninitCell (AssrtPointer sym wptr s, WI.SymBV sym wptr)
  | Size (LabelNonce s, WI.SymBV sym wptr) -- TODO: Maybe bounds is preferred for compositional reasoning
  | IsPtr (PtrVar argTys wptr s, AssrtPointer sym wptr s) -- we either are constructing this against a var or an index of an arg..., this is because the only pointers can
  -- be existentials or prepopulated we cant make a Ptr

data ConstructableAssertion sym wptr argTys s = ConstructableAssertion
  { constructableAssertion :: Assertion sym wptr argTys s
  , constructionBindings :: Bindings sym argTys
  }

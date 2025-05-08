{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.AssertProperty
  ( addPCBoundAssertion
  , addNoDynJumpAssertion
  ) where

import Control.Applicative (pure)
import Control.Monad (foldM)
import Data.Function (($))
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import Data.Traversable (for)
import Prelude (Bool(False), toInteger)

-- bv-sized
import Data.BitVector.Sized qualified as BV

-- parameterized-utils
import Data.Parameterized.Map qualified as MapF

-- what4
import What4.Interface qualified as W4
import What4.ProgramLoc qualified as W4

-- crucible
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Expr qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.Utils.RegRewrite qualified as C

-- crucible-llvm
import Lang.Crucible.LLVM.MemModel qualified as Mem

-- what4
import What4.FunctionName qualified as W4

-- macaw-base
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Types qualified as MT

-- macaw-symbolic
import Data.Macaw.Symbolic qualified as Symbolic

import Grease.Macaw.Arch
import Grease.Utility (segoffToAbsoluteAddr)

def ::
  C.TypeRepr ty ->
  C.Reg.AtomValue ext src ty ->
  C.Rewriter ext s src tgt a (C.Reg.Atom src ty)
def repr expr = do
  atom <- C.freshAtom repr
  C.addInternalStmt $ C.Reg.DefineAtom atom $ expr
  pure atom

-- | Convert a bitvector 'C.Reg.Atom' to a pointer using 'Symbolic.BitsToPtr'.
defBitsToPtr ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  MT.NatRepr (MC.ArchAddrWidth arch) ->
  C.Reg.Atom src (C.BVType (MC.ArchAddrWidth arch)) ->
  C.Rewriter (Symbolic.MacawExt arch) s src tgt a (C.Reg.Atom src (Mem.LLVMPointerType (MC.ArchAddrWidth arch)))
defBitsToPtr w bits =
  def Mem.PtrRepr $ C.Reg.EvalApp $ C.ExtensionApp
                  $ Symbolic.BitsToPtr w bits

-- | Convert a pointer 'C.Reg.Atom' to a bitvector using 'Symbolic.PtrToBits'.
defPtrToBits ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  MT.NatRepr (MC.ArchAddrWidth arch) ->
  C.Reg.Atom src (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.Rewriter (Symbolic.MacawExt arch) s src tgt a (C.Reg.Atom src (C.BVType (MC.ArchAddrWidth arch)))
defPtrToBits w ptr =
  def (C.BVRepr w) $ C.Reg.EvalApp $ C.ExtensionApp
                   $ Symbolic.PtrToBits w ptr

-- | Take an 'C.Reg.Atom' corresponding to the current program counter value and
-- convert it to a pointer containing an absolute address (i.e., a block number
-- of zero). This should be used rather than the raw program counter value
-- whenever making comparisons against other absolute addresses. See
-- @Note [Comparing the current program counter value]@.
defPcAbsAddr ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  MT.NatRepr (MC.ArchAddrWidth arch) ->
  C.Reg.Atom src (Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    {- ^ The current program counter value -} ->
  C.Rewriter (Symbolic.MacawExt arch) s src tgt a (C.Reg.Atom src (Mem.LLVMPointerType (MC.ArchAddrWidth arch)))
defPcAbsAddr w pc = do
  pcAbsAddr <- defPtrToBits w pc
  defBitsToPtr w pcAbsAddr

-- | Define an 'C.Reg.Atom' by converting an absolute address (represented as a
-- 'MC.MemWord') to a pointer using 'Symbolic.BitsToPtr'.
--
-- This is useful for constructing 'Mem.LLVMPtr's that you want to check for
-- equality against the current value of the program counter. See
-- @Note [Comparing the current program counter value]@.
defMemWordAbsAddr ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  MT.NatRepr (MC.ArchAddrWidth arch) ->
  MC.MemWord (MC.ArchAddrWidth arch)
    {- ^ The absolute address -} ->
  C.Rewriter (Symbolic.MacawExt arch) s src tgt a (C.Reg.Atom src (Mem.LLVMPointerType (MC.ArchAddrWidth arch)))
defMemWordAbsAddr w absAddr = do
  absAddrBV <-
    def (C.BVRepr w) $ C.Reg.EvalApp $ C.BVLit w $ BV.mkBV w
                     $ toInteger absAddr
  defBitsToPtr w absAddrBV

strLit :: Text -> C.Rewriter ext s src tgt a (C.Reg.Atom src (C.StringType C.Unicode))
strLit lit =
  def (C.StringRepr W4.UnicodeRepr)
      (C.Reg.EvalApp $ C.StringLit $ W4.UnicodeLiteral lit)

addAssert ::
  W4.Position ->
  -- | Error message
  Text ->
  C.Reg.AtomValue ext src C.BoolType ->
  C.Rewriter ext s src tgt a ()
addAssert pos msg expr = do
  s <- strLit msg
  b <- def C.BoolRepr expr
  C.addStmt $ W4.Posd pos $ C.Reg.Assert b s

-- | Assert that the PC is within the bounds of the @.text@ section, which
-- powers GREASE's @in-text@ check. We carve out exceptions for PLT stub
-- addresses, which are not within the @.text@ section but are nevertheless
-- uninteresting from an @in-text@ perspective.
addPCBoundAssertion ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , C.OrdF (MC.ArchReg arch)
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  MT.NatRepr (MC.ArchAddrWidth arch) ->
  MC.ArchReg arch (MT.BVType (MC.ArchAddrWidth arch)) ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  MC.MemWord (MC.ArchAddrWidth arch) ->
  MC.MemWord (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names
  Map.Map (MC.ArchSegmentOff arch) W4.FunctionName ->
  ArchRegCFG arch ->
  ArchRegCFG arch
addPCBoundAssertion w pcreg mem lower upper pltStubs = C.annotateCFGStmts ()
  (\pstmt -> do
      case W4.pos_val pstmt of
        C.Reg.DefineAtom _ (C.Reg.EvalExt (Symbolic.MacawArchStateUpdate _ ru))
          | Just (Symbolic.MacawCrucibleValue pc) <- MapF.lookup pcreg ru -> do
              let pos = W4.pos pstmt
              pcAbsAddrPtr <- defPcAbsAddr w pc

              -- Create a predicate that the PC is a PLT stub address.
              pltStubPtrs <-
                for (Map.keys pltStubs) $ \pltStubOff ->
                  defMemWordAbsAddr w $ segoffToAbsoluteAddr mem pltStubOff
              falseAtom <-
                def C.BoolRepr $ C.Reg.EvalApp $ C.BoolLit False
              pltStubPred <-
                foldM
                  (\acc pltStubPtr -> do
                    isPltStub <-
                      def C.BoolRepr $ C.Reg.EvalExt $
                      Symbolic.PtrEq (MC.addrWidthRepr w) pcAbsAddrPtr pltStubPtr
                    def C.BoolRepr $ C.Reg.EvalApp $ C.Or isPltStub acc)
                  falseAtom
                  pltStubPtrs

              -- Next, create predicates that the PC is strictly within the
              -- bounds of the .text section.
              lowerbound <- defMemWordAbsAddr w lower
              upperbound <- defMemWordAbsAddr w upper
              gtTextStartPred <-
                def C.BoolRepr $ C.Reg.EvalExt $
                Symbolic.PtrLeq (MC.addrWidthRepr w) lowerbound pcAbsAddrPtr
              ltTextStartPred <-
                def C.BoolRepr $ C.Reg.EvalExt $
                Symbolic.PtrLeq (MC.addrWidthRepr w) pcAbsAddrPtr upperbound

              -- Finally, assert the various predicates.
              addAssert pos "PC must be greater than .text start\n" $
                C.Reg.EvalApp $ C.Or pltStubPred gtTextStartPred
              addAssert pos "PC must be less than .text end\n" $
                C.Reg.EvalApp $ C.Or pltStubPred ltTextStartPred

              C.addStmt pstmt
        _ -> C.addStmt pstmt
  )
  (\_pstmt ->
      pure ()
  )

-- | Ensure that we are never updating the PC to be equal to a "bad" address
-- that shouldn't be jumped to.
addNoDynJumpAssertion ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , C.OrdF (MC.ArchReg arch)
  , 1 C.<= MC.ArchAddrWidth arch
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  MT.NatRepr (MC.ArchAddrWidth arch) ->
  MC.ArchReg arch (MT.BVType (MC.ArchAddrWidth arch)) ->
  -- ^ The PC for arch
  MC.Memory (MC.ArchAddrWidth arch) ->
  -- ^ The program memory
  MC.ArchSegmentOff arch ->
  -- ^ The address we don't want to jump to
  ArchRegCFG arch ->
  ArchRegCFG arch
addNoDynJumpAssertion w pcreg mem badAddrOff = C.annotateCFGStmts ()
  (\pstmt -> do
      case W4.pos_val pstmt of
        C.Reg.DefineAtom _ (C.Reg.EvalExt (Symbolic.MacawArchStateUpdate _ ru))
          | Just (Symbolic.MacawCrucibleValue pc) <- MapF.lookup pcreg ru -> do
              let pos = W4.pos pstmt
              pcAbsAddrPtr <- defPcAbsAddr w pc

              badAddrPtr <- defMemWordAbsAddr w $ segoffToAbsoluteAddr mem badAddrOff
              pcBad <-
                def C.BoolRepr $
                C.Reg.EvalExt $ Symbolic.PtrEq (MC.addrWidthRepr w) pcAbsAddrPtr badAddrPtr

              addAssert pos "Cannot call mprotect\n" $
                C.Reg.EvalApp $ C.Not pcBad

              C.addStmt pstmt
        _ -> C.addStmt pstmt
  )
  (\_pstmt ->
      pure ()
  )

{-
Note [Comparing the current program counter value]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grease's implementations of the in-text and no-mprotect checks require
comparing the current value of the program counter against another address.
While this sounds simple in theory, it is surprisingly subtle in practice. The
difficulty is that the program counter value is given as an LLVMPtr, and the
meaning of this LLVMPtr depends on how the program counter value was created:

1. If the program counter value was created using a BitsToPtr operation, then
   the resulting LLVMPtr's block number will be zero. That is, it will be a raw
   bitvector.
2. If the program counter value was created using a MacawGlobalPtr operation,
   then the resulting LLVMPtr's block number will be non-zero, as it will be
   represented as an offset from the LLVMPtr that backs the binary's global
   address space. That is, it will be a non-bitvector.

What's more, it is difficult to predict in advance whether the current program
counter value will be of type (1) or type (2), and I have observed different
types across different architectures. For example, AArch32 tends to produce
type-(1) LLVMPtrs, whereas PowerPC tends to produce type-(2) LLVMPtrs. The
distinction between the two types matters when comparing the current program
counter value to another address, because if the other address's LLVMPtr uses a
different block number, then the comparison will always fail!

This is all a bit confusing, but we can make things less confusing by ensuring
that we are always comparing absolute addresses. To do so, we:

* Convert the LLVMPtr in the program counter to a SymBV using PtrToBits (more
  on this part later).

* Convert the SymBV from the program counter back into an LLVMPtr with a block
  number of zero using BitsToPtr.

* Convert the other address to an LLVMPtr to an LLVMPtr with a block number of
  zero using BitsToPtr.

* Compare the two LLVMPtrs, knowing that they both have the same block number
  (0).

Using PtrToBits on the LLVMPtr from the program counter might seem somewhat
fishy because that LLVMPtr might have a non-zero block number, and
macaw-symbolic's default interpretation of PtrToBits is to throw an assertion
failure when given a non-zero block number. In the context of grease, however,
this is fine, as grease overrides PtrToBits to prevent throwing an assertion
failure when given a non-zero block number (see extensionEval in
Grease.Macaw.SimulatorHooks). Moreover, I'm not aware of any other way to
convert a pointer to a bitvector using only Macaw's statement extensions, so
PtrToBits will have to do.
-}

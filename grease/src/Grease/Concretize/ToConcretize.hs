{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- This module defines an interface for tracking symbolic values that are created
-- during simulation (generally by overrides) and should be concretized when a
-- goal fails.
--
-- It stores to-be-concretized values in a Crucible global variable of type
-- 'ToConcretizeType'. This variable is stored in the Crucible personality
-- (see 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality'),
-- and read-only access is provided via 'HasToConcretize'.
-- See this thread for a discussion of this choice:
-- <https://github.com/GaloisInc/grease/pull/203#discussion_r2132431744>.
module Grease.Concretize.ToConcretize (
  ToConcretizeType,
  HasToConcretize (toConcretize),
  newToConcretize,
  readToConcretize,
  stateToConcretize,
  addToConcretize,
) where

import Control.Lens qualified as Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class qualified as State
import Data.Maybe qualified as Maybe
import Data.Parameterized.Classes (knownRepr)
import Data.Parameterized.Context qualified as Ctx
import Data.Text (Text)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Generator qualified as LCCG
import Lang.Crucible.FunctionHandle qualified as LCF
import Lang.Crucible.Simulator qualified as LCS
import Lang.Crucible.Simulator.ExecutionTree qualified as LCSE
import Lang.Crucible.Simulator.GlobalState qualified as LCSG
import Lang.Crucible.Simulator.SymSequence qualified as LCSS
import Lang.Crucible.Types qualified as LCT
import What4.Expr.Builder qualified as WEB
import What4.Expr.GroundEval qualified as WEG
import What4.Interface qualified as WI

-- | The type of a global variable containing data to be concretized.
--
-- The first component of the struct of type 'LCT.StringType' is a name
-- (generally expected, but not required, to be concrete), and the second
-- component of type 'LCT.AnyType' is the value to be concretized.
type ToConcretizeType =
  LCT.SequenceType (LCT.StructType (Ctx.EmptyCtx Ctx.::> LCT.AnyType Ctx.::> LCT.StringType WI.Unicode))

-- | A class for Crucible personality types @p@ (see
-- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality') which contain a
-- @'C.GlobalVar' 'ToConcretize'@.
class HasToConcretize p where
  toConcretize :: p -> LCS.GlobalVar ToConcretizeType

instance HasToConcretize (LCS.GlobalVar ToConcretizeType) where
  toConcretize = id

-- | Create and initialize a @'LCS.GlobalVar' 'ToConcretizeType'@.
newToConcretize ::
  LCF.HandleAllocator ->
  LCSG.SymGlobalState sym ->
  IO (LCS.GlobalVar ToConcretizeType, LCSG.SymGlobalState sym)
newToConcretize halloc globals = do
  g <- LCCG.freshGlobalVar halloc "to-concretize" knownRepr
  pure (g, LCSG.insertGlobal g LCSS.SymSequenceNil globals)

-- | Extract the 'LCSG.SymGlobalState' from an 'LCSE.ExecResult'.
execResultGroundGlobals ::
  CB.IsSymInterface sym =>
  (sym ~ WEB.ExprBuilder t st fs) =>
  WEG.GroundEvalFn t ->
  LCSE.ExecResult p sym ext rtp ->
  IO (LCSG.SymGlobalState sym)
execResultGroundGlobals groundFn =
  LCSE.execResultGlobals $ \_simCtx _loc p l r -> do
    b <- WEG.groundEval groundFn p
    pure $ if b then l else r

-- | Read the value of the global variable of type 'ToConcretizeType' from
-- a 'C.ExecResult'.
--
-- If the global is not present, returns 'C.SymSequenceNil'.
readToConcretize ::
  CB.IsSymInterface sym =>
  (sym ~ WEB.ExprBuilder t st fs) =>
  HasToConcretize p =>
  WEG.GroundEvalFn t ->
  LCS.ExecResult p sym ext r ->
  IO (LCS.RegValue sym ToConcretizeType)
readToConcretize groundFn result = do
  let simCtx = LCS.execResultContext result
  let toConcVar = simCtx Lens.^. LCS.cruciblePersonality . Lens.to toConcretize
  globs <- liftIO (execResultGroundGlobals groundFn result)
  pure (Maybe.fromMaybe LCSS.SymSequenceNil (LCSG.lookupGlobal toConcVar globs))

-- | Getter for the @'LCS.GlobalVar' 'ToConcretizeType'@ in the
-- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality'
stateToConcretize ::
  HasToConcretize p =>
  LCS.SimState p sym ext r f a -> LCS.GlobalVar ToConcretizeType
stateToConcretize =
  Lens.view (LCSE.stateContext . LCSE.cruciblePersonality . Lens.to toConcretize)

-- | Add a value to the @'LCS.GlobalVar' 'ToConcretizeType'@ in the
-- 'C.SimState'.
addToConcretize ::
  forall p sym ext rtp args ret ty.
  CB.IsSymInterface sym =>
  HasToConcretize p =>
  -- | Name to be displayed when pretty-printing
  Text ->
  LCS.RegEntry sym ty ->
  LCS.OverrideSim p sym ext rtp args ret ()
addToConcretize name0 ent = do
  concVar <- State.gets stateToConcretize
  LCS.ovrWithBackend $ \bak -> do
    let sym = CB.backendGetSym bak
    name <- liftIO (WI.stringLit sym (WI.UnicodeLiteral name0))
    let LCS.RegEntry ty val = ent
    let anyVal = LCS.AnyValue ty val
    LCS.modifyGlobal concVar $ \toConc -> liftIO $ do
      let struct = Ctx.Empty Ctx.:> LCS.RV anyVal Ctx.:> LCS.RV name
      toConc' <- LCSS.consSymSequence sym struct toConc
      p <- CB.getPathCondition bak
      toConc'' <- liftIO (LCSS.muxSymSequence sym p toConc' toConc)
      pure ((), toConc'')

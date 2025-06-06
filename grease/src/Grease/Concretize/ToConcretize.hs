{-|
Copyright        : (c) Galois, Inc. 2025
Maintainer       : GREASE Maintainers <grease@galois.com>

This module defines an interface for tracking symbolic values that are created
during simulation (generally by overrides) and should be concretized when a
goal fails.

It stores to-be-concretized values in a Crucible global variable of type
'ToConcretizeType'. This variable is stored in the Crucible personality (see
'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality'), and access is
provided via 'HasToConcretize'. TODO: Make it read-only.
See this thread for a discussion of this choice:
<https://github.com/GaloisInc/grease/pull/203#discussion_r2132431744>.
-}

module Grease.Concretize.ToConcretize
  ( ToConcretizeType
  , HasToConcretize(toConcretize)
  , readToConcretize
  , stateToConcretize
  , addToConcretize
  ) where

import Control.Lens qualified as Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class qualified as State
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Text (Text)
import Lang.Crucible.Backend qualified as LCB
import Lang.Crucible.Simulator qualified as LCS
import Lang.Crucible.Simulator.ExecutionTree qualified as LCSE
import Lang.Crucible.Simulator.GlobalState qualified as LCSG
import Lang.Crucible.Simulator.SymSequence qualified as LCSS
import Lang.Crucible.Types qualified as LCT
import What4.Interface qualified as WI

-- | The type of a global variable containing data to be concretized.
--
-- The first component of the struct of type 'LCT.StringType' is a name
-- (generally expected, but not required, to be concrete), and the second
-- component of type 'LCT.AnyType' is the value to be concretized.
type ToConcretizeType
  = LCT.SequenceType (LCT.StructType (Ctx.EmptyCtx Ctx.::> LCT.AnyType Ctx.::> LCT.StringType WI.Unicode))

-- | A class for Crucible personality types @p@ (see
-- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality') which contain a
-- @'C.GlobalVar' 'ToConcretize'@.
class HasToConcretize p where
  toConcretize :: Lens.Lens' p (LCS.GlobalVar ToConcretizeType)

instance HasToConcretize (LCS.GlobalVar ToConcretizeType) where
  toConcretize = id

-- | Read the value of the global variable of type 'ToConcretizeType' from
-- a 'C.ExecResult'.
--
-- If the global is not present, returns 'C.SymSequenceNil'.
readToConcretize ::
  LCB.IsSymInterface sym =>
  HasToConcretize p =>
  LCS.ExecResult p sym ext r ->
  IO (LCS.RegValue sym ToConcretizeType)
readToConcretize result = do
  let simCtx = LCS.execResultContext result
  let toConcVar = simCtx Lens.^. LCS.cruciblePersonality . toConcretize
  globs <- liftIO (LCSE.execResultGlobals result)
  pure (Maybe.fromMaybe LCSS.SymSequenceNil (LCSG.lookupGlobal toConcVar globs))

-- | `Lens.Lens'` for the 'ToConcretize' in the
-- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality'
stateToConcretize ::
  HasToConcretize p =>
  Lens.Lens' (LCS.SimState p sym ext r f a) (LCS.GlobalVar ToConcretizeType)
stateToConcretize = LCSE.stateContext . LCSE.cruciblePersonality . toConcretize

-- | Add a value to the 'ToConcretize' in the 'C.SimState'.
addToConcretize ::
  forall p sym ext rtp args ret ty.
  LCB.IsSymInterface sym =>
  HasToConcretize p =>
  -- | Name to be displayed when pretty-printing
  Text ->
  LCS.RegEntry sym ty ->
  LCS.OverrideSim p sym ext rtp args ret ()
addToConcretize name0 ent = do
  concVar <- State.gets (Lens.view stateToConcretize)
  LCS.ovrWithBackend $ \bak -> do
    let sym = LCB.backendGetSym bak
    name <- liftIO (WI.stringLit sym (WI.UnicodeLiteral name0))
    let LCS.RegEntry ty val = ent
    let anyVal = LCS.AnyValue ty val
    LCS.modifyGlobal concVar $ \toConc -> do
      let struct = Ctx.Empty Ctx.:> LCS.RV anyVal Ctx.:> LCS.RV name
      toConc' <- liftIO (LCSS.consSymSequence sym struct toConc)
      pure ((), toConc')

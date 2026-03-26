{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- The shared parts of all @grease@ personality types. Both the Macaw personality
-- ('Grease.Macaw.SimulatorState.GreaseSimulatorState') and the LLVM personality
-- ('Grease.LLVM.Personality.GreaseLLVMPersonality') embed a 'Personality'.
module Grease.Personality (
  -- * @Personality@
  Personality (..),
  mkPersonality,
  HasPersonality (..),

  -- * @HasMemVar@
  HasMemVar (..),
  simStateMemVar,
  execStateMemVar,

  -- * Lenses for @Personality@
  pMemVar,
  pDbgContext,
  pToConcretize,
  pServerSocketFds,
  pRefinementData,
) where

import Control.Lens (Lens', (^.))
import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Parameterized.Some (Some)
import GHC.TypeLits (type Natural)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Refine.RefinementData (RefinementData)
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.ExecutionTree qualified as ET
import Lang.Crucible.Types (CrucibleType)
import Lang.Crucible.Types qualified as CT (Ctx)

-- | The shared personality core for @grease@.
--
-- This type holds state that is common to both the Macaw and LLVM personality
-- types. See 'Lang.Crucible.Simulator.cruciblePersonality'.
--
-- Type parameters:
--
-- - @sym@: the symbolic backend expression type
-- - @bak@: the symbolic backend type
-- - @t@: the nonce generator scope
-- - @cExt@: debugger command extension type
-- - @ext@: the Crucible language extension (e.g., @'Data.Macaw.Symbolic.MacawExt' arch@ or @'Lang.Crucible.LLVM.LLVM'@)
-- - @ret@: the Crucible return type
-- - @argTys@: Crucible argument types for the target function
-- - @wptr@: pointer width
type Personality ::
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  CrucibleType ->
  CT.Ctx CrucibleType ->
  Natural ->
  Type
data Personality sym bak t cExt ext ret argTys wptr = Personality
  { _pMemVar :: CS.GlobalVar CLM.Mem
  -- ^ The global variable holding the LLVM memory model state.
  , _pDbgContext :: Dbg.Context cExt sym ext ret
  -- ^ The debugger context for interactive debugging.
  , _pToConcretize :: CS.GlobalVar ToConc.ToConcretizeType
  -- ^ Values created during runtime to be passed to the concretization
  -- functionality and generally displayed to the user.
  , _pServerSocketFds :: Map.Map Integer (Some GSN.ServerSocketInfo)
  -- ^ A map from registered socket file descriptors to their corresponding
  -- metadata. See @Note [The networking story]@ in
  -- "Grease.Overrides.Networking".
  , _pRefinementData :: RefinementData sym bak t ext argTys wptr
  -- ^ Refinement-specific state, used by both @grease@ and @screach@ during
  -- their respective refinement loops.
  }

makeLenses ''Personality

-- | Create a 'Personality' with the given configuration. The server socket
-- file descriptor map is initialized to 'Map.empty'.
mkPersonality ::
  CS.GlobalVar CLM.Mem ->
  Dbg.Context cExt sym ext ret ->
  CS.GlobalVar ToConc.ToConcretizeType ->
  RefinementData sym bak t ext argTys wptr ->
  Personality sym bak t cExt ext ret argTys wptr
mkPersonality memVar dbgCtx toConc refineData =
  Personality
    { _pMemVar = memVar
    , _pDbgContext = dbgCtx
    , _pToConcretize = toConc
    , _pServerSocketFds = Map.empty
    , _pRefinementData = refineData
    }

-- | A class for personality types that contain an LLVM memory model
-- 'CS.GlobalVar'.
class HasMemVar p where
  getMemVar :: p -> CS.GlobalVar CLM.Mem

instance HasMemVar (Personality sym bak t cExt ext ret argTys wptr) where
  getMemVar = Lens.view pMemVar

-- | Extract the memory model 'CS.GlobalVar' from a 'CS.SimState'.
simStateMemVar ::
  HasMemVar p =>
  CS.SimState p sym ext rtp f a ->
  CS.GlobalVar CLM.Mem
simStateMemVar st = getMemVar (st ^. CS.stateContext . CS.cruciblePersonality)

-- | Extract the memory model 'CS.GlobalVar' from an 'ET.ExecState'.
execStateMemVar ::
  HasMemVar p =>
  ET.ExecState p sym ext r ->
  CS.GlobalVar CLM.Mem
execStateMemVar st = getMemVar (ET.execStateContext st ^. CS.cruciblePersonality)

-- | A class for personality types that contain a 'Personality' core.
class HasMemVar p => HasPersonality p sym bak t cExt ext ret argTys wptr | p -> sym bak t cExt ext ret argTys wptr where
  personality :: Lens' p (Personality sym bak t cExt ext ret argTys wptr)

instance HasPersonality (Personality sym bak t cExt ext ret argTys wptr) sym bak t cExt ext ret argTys wptr where
  personality = id
  {-# INLINE personality #-}

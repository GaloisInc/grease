{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Personality (
  -- * @GreaseLLVMPersonality@
  GreaseLLVMPersonality (..),
  mkGreaseLLVMPersonality,
  HasGreaseLLVMPersonality (..),

  -- * Lenses for @GreaseLLVMPersonality@
  llvmPersonality,
  llvmRecordState,
  llvmReplayState,
) where

import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import GHC.TypeLits (type Natural)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Personality qualified as GP
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.LLVM (LLVM)
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Types (CrucibleType)
import Lang.Crucible.Types qualified as CT (Ctx)

-- | The Crucible state extension for holding LLVM-specific @grease@ state.
type GreaseLLVMPersonality ::
  -- @sym@: the symbolic backend expression type
  Type ->
  -- @bak@: the symbolic backend type
  Type ->
  -- @t@: the nonce generator scope
  Type ->
  -- @cExt@: debugger command extension type
  Type ->
  -- @ret@: the Crucible return type
  CrucibleType ->
  -- @argTys@: Crucible argument types for the target function
  CT.Ctx CrucibleType ->
  -- @wptr@: pointer width
  Natural ->
  Type
data GreaseLLVMPersonality sym bak t cExt ret argTys wptr
  = GreaseLLVMPersonality
  { _llvmPersonality :: GP.Personality sym bak t cExt LLVM ret argTys wptr
  -- ^ The shared personality core. See 'Grease.Personality.Personality'.
  , _llvmRecordState ::
      CR.RecordState
        (GreaseLLVMPersonality sym bak t cExt ret argTys wptr)
        sym
        LLVM
        (CS.RegEntry sym ret)
  , _llvmReplayState ::
      CR.ReplayState
        (GreaseLLVMPersonality sym bak t cExt ret argTys wptr)
        sym
        LLVM
        (CS.RegEntry sym ret)
  }

makeLenses ''GreaseLLVMPersonality

-- | Create a 'GreaseLLVMPersonality' with the given configuration.
mkGreaseLLVMPersonality ::
  forall sym bak t cExt ret argTys wptr p.
  (p ~ GreaseLLVMPersonality sym bak t cExt ret argTys wptr) =>
  GP.Personality sym bak t cExt LLVM ret argTys wptr ->
  CR.RecordState p sym LLVM (CS.RegEntry sym ret) ->
  CR.ReplayState p sym LLVM (CS.RegEntry sym ret) ->
  p
mkGreaseLLVMPersonality pers recState repState =
  GreaseLLVMPersonality
    { _llvmPersonality = pers
    , _llvmRecordState = recState
    , _llvmReplayState = repState
    }

class
  GP.HasPersonality p sym bak t cExt LLVM ret argTys wptr =>
  HasGreaseLLVMPersonality p sym bak t cExt ret argTys wptr
    | p -> sym bak t cExt ret argTys wptr
  where
  greaseLlvmPersonality :: Lens' p (GreaseLLVMPersonality sym bak t cExt ret argTys wptr)

instance HasGreaseLLVMPersonality (GreaseLLVMPersonality sym bak t cExt ret argTys wptr) sym bak t cExt ret argTys wptr where
  greaseLlvmPersonality = id
  {-# INLINE greaseLlvmPersonality #-}

instance Dbg.HasContext (GreaseLLVMPersonality sym bak t cExt ret argTys wptr) cExt sym LLVM ret where
  context = llvmPersonality . GP.pDbgContext
  {-# INLINE context #-}

instance GP.HasMemVar (GreaseLLVMPersonality sym bak t cExt ret argTys wptr) where
  getMemVar = Lens.view (llvmPersonality . GP.pMemVar)

instance ToConc.HasToConcretize (GreaseLLVMPersonality sym bak t cExt ret argTys wptr) where
  toConcretize = Lens.view (llvmPersonality . GP.pToConcretize)

instance GSN.HasServerSocketFds (GreaseLLVMPersonality sym bak t cExt ret argTys wptr) where
  serverSocketFdsL = llvmPersonality . GP.pServerSocketFds

instance GP.HasPersonality (GreaseLLVMPersonality sym bak t cExt ret argTys wptr) sym bak t cExt LLVM ret argTys wptr where
  personality = llvmPersonality
  {-# INLINE personality #-}

instance
  CR.HasRecordState
    (GreaseLLVMPersonality sym bak t cExt ret argTys wptr)
    (GreaseLLVMPersonality sym bak t cExt ret argTys wptr)
    sym
    LLVM
    (CS.RegEntry sym ret)
  where
  recordState = llvmRecordState
  {-# INLINE recordState #-}

instance
  CR.HasReplayState
    (GreaseLLVMPersonality sym bak t cExt ret argTys wptr)
    (GreaseLLVMPersonality sym bak t cExt ret argTys wptr)
    sym
    LLVM
    (CS.RegEntry sym ret)
  where
  replayState = llvmReplayState
  {-# INLINE replayState #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Personality qualified as GP
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.LLVM (LLVM)
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Types (CrucibleType)

-- | The Crucible state extension for holding LLVM-specific @grease@ state.
type GreaseLLVMPersonality ::
  -- @cExt@: debugger command extension type
  Type ->
  -- @sym@: the symbolic backend expression type
  Type ->
  -- @ret@: the Crucible return type
  CrucibleType ->
  Type
data GreaseLLVMPersonality cExt sym ret
  = GreaseLLVMPersonality
  { _llvmPersonality :: GP.Personality cExt sym LLVM ret
  -- ^ The shared personality core. See 'Grease.Personality.Personality'.
  , _llvmRecordState ::
      CR.RecordState
        (GreaseLLVMPersonality cExt sym ret)
        sym
        LLVM
        (CS.RegEntry sym ret)
  , _llvmReplayState ::
      CR.ReplayState
        (GreaseLLVMPersonality cExt sym ret)
        sym
        LLVM
        (CS.RegEntry sym ret)
  }

makeLenses ''GreaseLLVMPersonality

-- | Create a 'GreaseLLVMPersonality' with the given configuration.
mkGreaseLLVMPersonality ::
  forall cExt sym ret p.
  (p ~ GreaseLLVMPersonality cExt sym ret) =>
  GP.Personality cExt sym LLVM ret ->
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
  HasGreaseLLVMPersonality p cExt sym ret
    | p -> cExt sym ret
  where
  greaseLlvmPersonality :: Lens' p (GreaseLLVMPersonality cExt sym ret)

instance HasGreaseLLVMPersonality (GreaseLLVMPersonality cExt sym ret) cExt sym ret where
  greaseLlvmPersonality = id
  {-# INLINE greaseLlvmPersonality #-}

instance Dbg.HasContext (GreaseLLVMPersonality cExt sym ret) cExt sym LLVM ret where
  context = llvmPersonality . GP.pDbgContext
  {-# INLINE context #-}

instance ToConc.HasToConcretize (GreaseLLVMPersonality cExt sym ret) where
  toConcretize = Lens.view (llvmPersonality . GP.pToConcretize)

instance GSN.HasServerSocketFds (GreaseLLVMPersonality cExt sym ret) where
  serverSocketFdsL = llvmPersonality . GP.pServerSocketFds

instance GP.HasPersonality (GreaseLLVMPersonality cExt sym ret) cExt sym LLVM ret where
  personality = llvmPersonality
  {-# INLINE personality #-}

instance
  CR.HasRecordState
    (GreaseLLVMPersonality cExt sym ret)
    (GreaseLLVMPersonality cExt sym ret)
    sym
    LLVM
    (CS.RegEntry sym ret)
  where
  recordState = llvmRecordState
  {-# INLINE recordState #-}

instance
  CR.HasReplayState
    (GreaseLLVMPersonality cExt sym ret)
    (GreaseLLVMPersonality cExt sym ret)
    sym
    LLVM
    (CS.RegEntry sym ret)
  where
  replayState = llvmReplayState
  {-# INLINE replayState #-}

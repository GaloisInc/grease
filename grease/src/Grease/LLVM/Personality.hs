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
  llvmDbgContext,
  llvmToConcretize,
  llvmServerSocketFds,
  llvmRecordState,
  llvmReplayState,
) where

import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Parameterized.Some (Some)
import Grease.Concretize.ToConcretize qualified as ToConc
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
  { _llvmDbgContext :: Dbg.Context cExt sym LLVM ret
  , _llvmToConcretize :: CS.GlobalVar ToConc.ToConcretizeType
  -- ^ Values created during runtime to be passed to the concretization
  -- functionality and generally displayed to the user.
  , _llvmServerSocketFds :: Map.Map Integer (Some GSN.ServerSocketInfo)
  -- ^ A map from registered socket file descriptors to their corresponding
  -- metadata. See @Note [The networking story]@ in
  -- "Grease.Overrides.Networking".
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
  Dbg.Context cExt sym LLVM ret ->
  CS.GlobalVar ToConc.ToConcretizeType ->
  CR.RecordState p sym LLVM (CS.RegEntry sym ret) ->
  CR.ReplayState p sym LLVM (CS.RegEntry sym ret) ->
  p
mkGreaseLLVMPersonality dbgCtx toConcVar recState repState =
  GreaseLLVMPersonality
    { _llvmDbgContext = dbgCtx
    , _llvmToConcretize = toConcVar
    , _llvmServerSocketFds = Map.empty
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
  context = llvmDbgContext
  {-# INLINE context #-}

instance ToConc.HasToConcretize (GreaseLLVMPersonality cExt sym ret) where
  toConcretize = Lens.view llvmToConcretize

instance GSN.HasServerSocketFds (GreaseLLVMPersonality cExt sym ret) where
  serverSocketFdsL = llvmServerSocketFds

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

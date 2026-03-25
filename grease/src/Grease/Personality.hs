{-# LANGUAGE DataKinds #-}
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
  HasPersonality (..),

  -- * Lenses for @Personality@
  pDbgContext,
  pToConcretize,
  pServerSocketFds,
) where

import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Parameterized.Some (Some)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types (CrucibleType)

-- | The shared personality core for @grease@.
--
-- This type holds state that is common to both the Macaw and LLVM personality
-- types. See 'Lang.Crucible.Simulator.cruciblePersonality'.
--
-- Type parameters:
--
-- - @cExt@: debugger command extension type
-- - @sym@: the symbolic backend expression type
-- - @ext@: the Crucible language extension (e.g., @'Data.Macaw.Symbolic.MacawExt' arch@ or @'Lang.Crucible.LLVM.LLVM'@)
-- - @ret@: the Crucible return type
type Personality ::
  Type ->
  Type ->
  Type ->
  CrucibleType ->
  Type
data Personality cExt sym ext ret = Personality
  { _pDbgContext :: Dbg.Context cExt sym ext ret
  -- ^ The debugger context for interactive debugging.
  , _pToConcretize :: CS.GlobalVar ToConc.ToConcretizeType
  -- ^ Values created during runtime to be passed to the concretization
  -- functionality and generally displayed to the user.
  , _pServerSocketFds :: Map.Map Integer (Some GSN.ServerSocketInfo)
  -- ^ A map from registered socket file descriptors to their corresponding
  -- metadata. See @Note [The networking story]@ in
  -- "Grease.Overrides.Networking".
  }

makeLenses ''Personality

-- | A class for personality types that contain a 'Personality' core.
class HasPersonality p cExt sym ext ret | p -> cExt sym ext ret where
  personality :: Lens' p (Personality cExt sym ext ret)

instance HasPersonality (Personality cExt sym ext ret) cExt sym ext ret where
  personality = id
  {-# INLINE personality #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Personality (
  -- * @GreaseLLVMPersonality@
  GreaseLLVMPersonality (..),
  HasGreaseLLVMPersonality (..),
) where

import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import Grease.Concretize.ToConcretize qualified as ToConc
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.LLVM (LLVM)
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types (CrucibleType)

type GreaseLLVMPersonality :: Type -> Type -> CrucibleType -> Type
data GreaseLLVMPersonality cExt sym t
  = GreaseLLVMPersonality
  { _dbgContext :: Dbg.Context cExt sym LLVM t
  , _toConcretize :: CS.GlobalVar ToConc.ToConcretizeType
  -- ^ Values created during runtime to be passed to the concretization
  -- functionality and generally displayed to the user.
  }

makeLenses ''GreaseLLVMPersonality

class
  HasGreaseLLVMPersonality p cExt sym t
    | p -> cExt sym t
  where
  greaseLlvmPersonality :: Lens' p (GreaseLLVMPersonality cExt sym t)

instance HasGreaseLLVMPersonality (GreaseLLVMPersonality cExt sym t) cExt sym t where
  greaseLlvmPersonality = id
  {-# INLINE greaseLlvmPersonality #-}

instance Dbg.HasContext (GreaseLLVMPersonality cExt sym t) cExt sym LLVM t where
  context = dbgContext
  {-# INLINE context #-}

instance ToConc.HasToConcretize (GreaseLLVMPersonality cExt sym t) where
  toConcretize = Lens.view toConcretize

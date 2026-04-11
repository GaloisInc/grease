{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- The 'RefinementData' and 'RefinementInputs' types.
module Grease.Refine.RefinementData (
  RefinementInputs (..),
  RefinementData (..),
) where

import Data.IORef (IORef)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Nonce (Nonce)
import GHC.TypeLits (type Natural)
import Grease.Concretize qualified as Conc
import Grease.ErrorDescription (ErrorDescription)
import Grease.Heuristic (RefineHeuristic)
import Grease.Setup.Annotations qualified as Anns
import Grease.Shape (ArgShapes)
import Grease.Shape.NoTag (NoTag)
import Grease.Solver (Solver)
import Grease.ValueName (ValueName)
import Lang.Crucible.Types qualified as C
import Lang.Crucible.Utils.Timeout qualified as C

-- | The caller-provided inputs to the refinement process: the things that are
-- known up-front and drive how refinement behaves.
--
-- Type parameters:
--
-- - @sym@: the symbolic backend expression type
-- - @bak@: the symbolic backend type
-- - @ext@: the Crucible language extension
-- - @argTys@: Crucible argument types for the target function
type RefinementInputs :: Type -> Type -> Type -> Ctx.Ctx C.CrucibleType -> Type
data RefinementInputs sym bak ext argTys
  = RefinementInputs
  { refineInputArgNames :: Ctx.Assignment ValueName argTys
  -- ^ Names of the function arguments.
  , refineInputArgShapes :: ArgShapes ext NoTag argTys
  -- ^ Current argument shapes being refined.
  , refineInputHeuristics :: [RefineHeuristic sym bak ext argTys]
  -- ^ Heuristics used to guide refinement.
  , refineInputSolver :: Solver
  -- ^ The solver to use for discharging proof obligations.
  }

-- | Data needed for refinement.
--
-- Type parameters:
--
-- - @sym@: the symbolic backend expression type
-- - @bak@: the symbolic backend type
-- - @t@: the nonce generator scope
-- - @ext@: the Crucible language extension
-- - @argTys@: Crucible argument types for the target function
-- - @wptr@: pointer width
type RefinementData :: Type -> Type -> Type -> Type -> Ctx.Ctx C.CrucibleType -> Natural -> Type
data RefinementData sym bak t ext argTys wptr
  = RefinementData
  { refineInputs :: RefinementInputs sym bak ext argTys
  , refineAnns :: Anns.Annotations sym ext argTys
  , refineInitState :: Conc.InitialState sym ext argTys wptr
  , refineSolverTimeout :: C.Timeout
  , refineErrMap :: IORef (Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym))
  }

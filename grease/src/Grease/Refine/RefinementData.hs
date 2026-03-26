{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- The 'RefinementData' type.
module Grease.Refine.RefinementData (
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
  { refineAnns :: Anns.Annotations sym ext argTys
  , refineArgNames :: Ctx.Assignment ValueName argTys
  , refineArgShapes :: ArgShapes ext NoTag argTys
  , refineHeuristics :: [RefineHeuristic sym bak ext argTys]
  , refineInitState :: Conc.InitialState sym ext argTys wptr
  , refineSolver :: Solver
  , refineSolverTimeout :: C.Timeout
  , refineErrMap :: IORef (Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym))
  }

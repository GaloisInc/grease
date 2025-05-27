{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Grease.Solver
  ( Solver(..)
  , solverAdapter
  , withSolverOnlineBackend
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Parameterized.Nonce (NonceGenerator)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Online qualified as C
import What4.Expr qualified as W4
import What4.ProblemFeatures qualified as W4
import What4.Protocol.Online qualified as W4
import What4.Solver qualified as W4

-- | The SMT solver to use for solving proof goals.
data Solver
  = CVC4
  | CVC5
  | Yices
  | Z3
  deriving (Bounded, Enum, Read, Show)

-- | Get the 'W4.SolverAdapter' for the requested 'Solver'.
solverAdapter :: Solver -> W4.SolverAdapter t
solverAdapter solver =
  case solver of
    CVC4  -> W4.cvc4Adapter
    CVC5  -> W4.cvc5Adapter
    Yices -> W4.yicesAdapter
    Z3    -> W4.z3Adapter

-- | Do something with an online backend for the requested 'Solver'.
withSolverOnlineBackend ::
  forall m a fm scope.
  (MonadIO m, MonadMask m) =>
  Solver ->
  W4.FloatModeRepr fm ->
  NonceGenerator IO scope ->
  ( forall sym bak solver st.
    ( sym ~ W4.ExprBuilder scope st (W4.Flags fm)
    , bak ~ C.OnlineBackend solver scope st (W4.Flags fm)
    , C.IsSymBackend sym bak
    , W4.OnlineSolver solver
    ) =>
    bak ->
    m a
  ) ->
  m a
withSolverOnlineBackend solver fm ng bakAction =
  case solver of
    CVC4 ->
      withSym $ \sym ->
      C.withCVC4OnlineBackend sym unsatFeatures problemFeatures bakAction
    CVC5 ->
      withSym $ \sym ->
      C.withCVC5OnlineBackend sym unsatFeatures problemFeatures bakAction
    Yices ->
      withSym $ \sym ->
      C.withYicesOnlineBackend sym unsatFeatures problemFeatures bakAction
    Z3 ->
      withSym $ \sym ->
      C.withZ3OnlineBackend sym unsatFeatures problemFeatures bakAction
  where
    withSym ::
      ( forall sym st.
        ( sym ~ W4.ExprBuilder scope st (W4.Flags fm)
        , C.IsSymInterface sym
        ) =>
        sym ->
        m a
      ) ->
      m a
    withSym symAction = do
      sym <- liftIO $ W4.newExprBuilder fm W4.EmptyExprBuilderState ng
      -- In order for GHC to conclude that `IsSymInterface sym` holds, we must
      -- first prove that there is an `IsInterpretedFloatExprBuilder sym`
      -- instance in scope. Each instance of this class matches on a particular
      -- `FloatMode` type, however. As such, the most direct way to convince
      -- GHC's instance solver is to pattern match on all possible cases of
      -- `FloatModeRepr`, which we do below.
      case fm of
        W4.FloatIEEERepr ->
          symAction sym
        W4.FloatRealRepr ->
          symAction sym
        W4.FloatUninterpretedRepr ->
          symAction sym

    unsatFeatures :: C.UnsatFeatures
    unsatFeatures = C.NoUnsatFeatures

    problemFeatures :: W4.ProblemFeatures
    problemFeatures = W4.noFeatures

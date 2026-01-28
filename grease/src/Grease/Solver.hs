{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Solver (
  Solver (..),
  solverAdapter,
  withSolverOnlineBackend,
) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Parameterized.Nonce (NonceGenerator)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as C
import What4.Expr qualified as WE
import What4.ProblemFeatures qualified as W4
import What4.Protocol.Online qualified as WPO
import What4.Solver qualified as W4

-- | The SMT solver to use for solving proof goals.
data Solver
  = Cvc4
  | Cvc5
  | Yices
  | Z3
  deriving (Bounded, Enum, Read, Show)

-- | Get the 'W4.SolverAdapter' for the requested 'Solver'.
solverAdapter :: Solver -> W4.SolverAdapter t
solverAdapter solver =
  case solver of
    Cvc4 -> W4.cvc4Adapter
    Cvc5 -> W4.cvc5Adapter
    Yices -> W4.yicesAdapter
    Z3 -> W4.z3Adapter

-- | Do something with an online backend for the requested 'Solver'.
withSolverOnlineBackend ::
  forall m a fm scope.
  (MonadIO m, MonadMask m) =>
  Solver ->
  WE.FloatModeRepr fm ->
  NonceGenerator IO scope ->
  ( forall sym bak solver st.
    ( sym ~ WE.ExprBuilder scope st (WE.Flags fm)
    , bak ~ C.OnlineBackend solver scope st (WE.Flags fm)
    , CB.IsSymBackend sym bak
    , WPO.OnlineSolver solver
    ) =>
    bak ->
    m a
  ) ->
  m a
withSolverOnlineBackend solver fm ng bakAction =
  case solver of
    Cvc4 ->
      withSym $ \sym ->
        C.withCVC4OnlineBackend sym unsatFeatures problemFeatures bakAction
    Cvc5 ->
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
      ( sym ~ WE.ExprBuilder scope st (WE.Flags fm)
      , CB.IsSymInterface sym
      ) =>
      sym ->
      m a
    ) ->
    m a
  withSym symAction = do
    sym <- liftIO $ WE.newExprBuilder fm WE.EmptyExprBuilderState ng
    -- In order for GHC to conclude that `IsSymInterface sym` holds, we must
    -- first prove that there is an `IsInterpretedFloatExprBuilder sym`
    -- instance in scope. Each instance of this class matches on a particular
    -- `FloatMode` type, however. As such, the most direct way to convince
    -- GHC's instance solver is to pattern match on all possible cases of
    -- `FloatModeRepr`, which we do below.
    case fm of
      WE.FloatIEEERepr ->
        symAction sym
      WE.FloatRealRepr ->
        symAction sym
      WE.FloatUninterpretedRepr ->
        symAction sym

  unsatFeatures :: C.UnsatFeatures
  unsatFeatures = C.NoUnsatFeatures

  problemFeatures :: W4.ProblemFeatures
  problemFeatures = W4.noFeatures

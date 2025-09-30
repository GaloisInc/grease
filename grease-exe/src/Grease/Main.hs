{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Grease.Main (main) where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Parameterized.NatRepr (knownNat)
import Grease.Diagnostic (GreaseLogAction)
import Grease.Heuristic
import Grease.LLVM qualified as LLVM
import Grease.LLVM.SetupHook qualified as LLVM (SetupHook)
import Grease.Output (BatchStatus)
import Grease.Refine (refineOnce, refinementLoop)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Extension qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import System.IO (IO)
import What4.Expr qualified as W4
import Prelude (Maybe (..), undefined)

-- | Define @?memOpts@ in a continuation, setting the 'Mem.laxLoadsAndStores'
-- option according to whether the user set the @--rust@ flag.
withMemOptions :: () -> ((?memOpts :: Mem.MemOptions) => r) -> r
withMemOptions _ _ = undefined

simulateLlvmCfg ::
  forall sym bak arch solver t st fm.
  ( Mem.HasPtrWidth (CLLVM.ArchWidth arch)
  , OnlineSolverAndBackend solver sym bak t st (W4.Flags fm)
  ) =>
  GreaseLogAction ->
  bak ->
  LLVM.SetupHook sym arch ->
  IO BatchStatus
simulateLlvmCfg la bak setupHook = do
  C.Refl <-
    case C.testEquality ?ptrWidth (knownNat @64) of
      Just r -> pure r
      Nothing -> undefined

  let ?recordLLVMAnnotation = \_ _ _ -> pure ()
  result <- withMemOptions undefined $ do
    let heuristics = [mustFailHeuristic]
    refinementLoop la undefined undefined undefined $ \_ _ ->
      refineOnce
        undefined
        undefined
        undefined
        bak
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        heuristics
        undefined
        $ \p _ _ _ ->
          LLVM.initState
            bak
            la
            undefined
            p
            undefined
            undefined
            undefined
            undefined
            undefined
            undefined
            undefined
            setupHook
            undefined
            undefined
            undefined

  pure undefined

main :: IO ()
main = pure ()

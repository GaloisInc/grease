{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Entrypoint (
  checkMacawEntrypointCfgsSignatures,
  MacawCfgTypecheckError (..),
  checkMacawCfgSignature,
)
where

import Control.Exception.Safe (throw)
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Some (Some (Some))
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Grease.Entrypoint qualified as GE
import Grease.Macaw (regStructRepr)
import Grease.Macaw.Arch (ArchContext)
import Grease.Utility (GreaseException (..))
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Types qualified as CT
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import What4.FunctionName qualified as W4

-- | Check that the CFGs in an 'GE.EntrypointCFGs' have the signatures of valid
-- Macaw CFGs (i.e., it takes the register struct as its only argument and
-- returns it). Throw 'GreaseException' if not.
checkMacawEntrypointCfgsSignatures ::
  ArchContext arch ->
  GE.EntrypointCfgs (C.Reg.AnyCFG (Symbolic.MacawExt arch)) ->
  IO
    ( GE.EntrypointCfgs
        ( C.Reg.SomeCFG
            (Symbolic.MacawExt arch)
            (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
            (Symbolic.ArchRegStruct arch)
        )
    )
checkMacawEntrypointCfgsSignatures archCtx entrypointCfgs = do
  GE.EntrypointCfgs
    { GE.entrypointStartupOv = mbStartupOv
    , GE.entrypointCfg = C.Reg.AnyCFG entrypointCfg0
    } <-
    pure entrypointCfgs
  let entryName = W4.functionName (C.handleName (C.Reg.cfgHandle entrypointCfg0))
  let name = Text.concat ["CFG `", entryName, "`"]
  C.Reg.SomeCFG entrypointCfg' <-
    throwMacawCfgTypecheckError $
      checkMacawCfgSignature archCtx name entrypointCfg0
  mbStartupOvSome <- for mbStartupOv $ \startupOv -> do
    GE.StartupOv
      { GE.startupOvCfg = C.Reg.AnyCFG startupOvCfg0
      , GE.startupOvForwardDecs = fwdDecs
      } <-
      pure startupOv
    let entryName' = W4.functionName (C.handleName (C.Reg.cfgHandle startupOvCfg0))
    let name' = Text.concat ["startup override for `", entryName', "`"]
    C.Reg.SomeCFG startupOvCfg' <-
      throwMacawCfgTypecheckError $
        checkMacawCfgSignature archCtx name' startupOvCfg0
    pure $
      GE.StartupOv
        { GE.startupOvCfg = C.Reg.SomeCFG startupOvCfg'
        , GE.startupOvForwardDecs = fwdDecs
        }
  pure $
    GE.EntrypointCfgs
      { GE.entrypointStartupOv = mbStartupOvSome
      , GE.entrypointCfg = C.Reg.SomeCFG entrypointCfg'
      }

throwMacawCfgTypecheckError ::
  Either
    MacawCfgTypecheckError
    ( C.Reg.SomeCFG
        (Symbolic.MacawExt arch)
        (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
        (Symbolic.ArchRegStruct arch)
    ) ->
  IO
    ( C.Reg.SomeCFG
        (Symbolic.MacawExt arch)
        (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
        (Symbolic.ArchRegStruct arch)
    )
throwMacawCfgTypecheckError =
  \case
    Right ok -> pure ok
    Left err -> do
      let url = "https://galoisinc.github.io/grease/sexp-progs.html"
      let msg = PP.renderStrict (PP.layoutPretty PP.defaultLayoutOptions (PP.pretty err))
      throw (GreaseException (msg <> "\n" <> "For more information, see " <> url))

-- | Possible errors resulting from 'checkMacawCfgSignature'.
data MacawCfgTypecheckError
  = BadArgs
      -- | Name
      Text.Text
      -- | Actual arguments
      (Some (Ctx.Assignment CT.TypeRepr))
  | BadRet
      -- | Name
      Text.Text
      -- | Actual arguments
      (Some CT.TypeRepr)

instance PP.Pretty MacawCfgTypecheckError where
  pretty =
    \case
      BadArgs name (Some argTys) ->
        let prettyArgs = PP.list $ TFC.toListFC PP.pretty argTys
         in PP.vcat
              [ PP.hsep ["Bad argument types for", PP.pretty name <> ":", prettyArgs]
              , "Expected a single argument, the struct of register values"
              ]
      BadRet name (Some retTy) ->
        PP.vcat
          [ PP.hsep ["Bad return type for", PP.pretty name <> ":", PP.pretty retTy]
          , "Expected the struct of register values"
          ]

-- | Check that the a CFG has the signatures of a valid Macaw CFG (i.e.,
-- it takes the register struct as its only argument and returns it).
checkMacawCfgSignature ::
  ArchContext arch ->
  -- | Name to use in error messages. A good default is the 'C.handleName'.
  Text.Text ->
  C.Reg.CFG (Symbolic.MacawExt arch) s init ret ->
  Either
    MacawCfgTypecheckError
    ( C.Reg.SomeCFG
        (Symbolic.MacawExt arch)
        (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
        (Symbolic.ArchRegStruct arch)
    )
checkMacawCfgSignature archCtx name regCfg = do
  let expectedArgTys = Ctx.singleton (regStructRepr archCtx)
  let expectedRet = regStructRepr archCtx
  let argTys = C.Reg.cfgArgTypes regCfg
  Refl <-
    case testEquality argTys expectedArgTys of
      Just r -> pure r
      Nothing ->
        Left (BadArgs name (Some argTys))
  let retTy = C.Reg.cfgReturnType regCfg
  Refl <-
    case testEquality retTy expectedRet of
      Just r -> pure r
      Nothing -> Left (BadRet name (Some retTy))
  pure (C.Reg.SomeCFG regCfg)

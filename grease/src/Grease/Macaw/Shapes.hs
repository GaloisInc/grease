{-# LANGUAGE ImplicitParams #-}

module Grease.Macaw.Shapes (
  macawInitArgShapes,
) where

import Control.Monad.Trans.Maybe qualified as MaybeT
import Data.ElfEdit qualified as Elf
import Data.Functor.Const (Const (..))
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Grease.Diagnostic (GreaseLogAction)
import Grease.Macaw (minimalArgShapes)
import Grease.Macaw.Arch (ArchContext, ArchReloc)
import Grease.Macaw.Dwarf (loadDwarfPreconditions)
import Grease.Options qualified as GO
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag)
import Grease.Shape.Simple qualified as Simple
import Grease.Utility (segoffToAbsoluteAddr)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Syntax.Concrete qualified as CSyn

-- | Compute the initial 'ArgShapes' for a Macaw CFG.
--
-- Sources argument shapes from:
--
-- 1. A default initial shape for each register (via 'minimalArgShapes')
-- 2. DWARF debug info (via 'loadDwarfPreconditions') if
--    'GO.initPrecondUseDebugInfo' is 'True'
-- 3. A shape DSL file (via the 'Shape.ParsedShapes')
-- 4. Simple shapes from the CLI (via 'Simple.useSimpleShapes')
--
-- Later steps override earlier ones.
macawInitArgShapes ::
  ( CB.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GreaseLogAction ->
  bak ->
  ArchContext arch ->
  GO.InitialPreconditionOpts ->
  Maybe (Shape.ParsedShapes (Symbolic.MacawExt arch)) ->
  Maybe (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch)) ->
  MM.Memory (MC.ArchAddrWidth arch) ->
  Ctx.Assignment (Const String) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)) ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  IO (Either Shape.TypeMismatch (Shape.ArgShapes (Symbolic.MacawExt arch) NoTag (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch))))
macawInitArgShapes la bak archCtx opts parsed elf memory argNames mbCfgAddr = do
  let mdEntryAbsAddr = fmap (segoffToAbsoluteAddr memory) mbCfgAddr
  initArgs0 <- minimalArgShapes bak archCtx mdEntryAbsAddr
  let shouldUseDwarf = GO.initPrecondUseDebugInfo opts
  let getDwarfArgs = do
        -- MaybeT IO
        elfHdr <- MaybeT.hoistMaybe elf
        addr <- MaybeT.hoistMaybe $ mbCfgAddr
        MaybeT.MaybeT $
          loadDwarfPreconditions
            la
            addr
            memory
            (GO.initPrecondTypeUnrollingBound opts)
            argNames
            initArgs0
            elfHdr
            archCtx
  dwarfedArgs <-
    if shouldUseDwarf
      then do
        v <- MaybeT.runMaybeT getDwarfArgs
        pure $ Maybe.fromMaybe initArgs0 v
      else pure initArgs0
  pure $ do
    -- Either
    initArgs1 <-
      case parsed of
        Nothing -> Right dwarfedArgs
        Just p -> Shape.replaceShapes argNames dwarfedArgs p
    Simple.useSimpleShapes argNames initArgs1 (GO.initPrecondSimpleShapes opts)

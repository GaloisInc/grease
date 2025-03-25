{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Grease.Macaw.Load
  ( LoadedProgram(..)
  , load
  ) where

import Control.Applicative (pure)
import Control.Exception.Safe (throw)
import Control.Monad (return, forM, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (Bool(..), (&&), (||), otherwise)
import qualified Data.ByteString as BS
import Data.Either (Either(..))
import Data.Eq (Eq(..))
import Data.Function (($), (&), (.))
import Data.Functor ((<$>), fmap)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe(..), fromMaybe, mapMaybe)
import Data.Ord (Ord(..))
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Traversable (Traversable(..))
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vec
import qualified Lumberjack as LJ
import Prelude (Integral(..), Num(..), fromIntegral)
import System.Directory (Permissions)
import System.FilePath (FilePath)
import System.IO (IO)
import Text.Read (readMaybe)

-- crucible
import qualified Lang.Crucible.CFG.Core as C

-- crucible-llvm
import           Lang.Crucible.LLVM.DataLayout (DataLayout)
import qualified Lang.Crucible.LLVM.MemModel as Mem
import qualified Lang.Crucible.LLVM.DataLayout as Mem

-- elf-edit
import qualified Data.ElfEdit as Elf

-- elf-edit-core-dump
import qualified Data.ElfEdit.CoreDump as CoreDump

-- macaw-loader
import qualified Data.Macaw.BinaryLoader as Loader
import qualified Data.Macaw.BinaryLoader.ELF as Loader

-- macaw-loader-aarch32
import Data.Macaw.BinaryLoader.AArch32 ()

-- macaw-base
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as EL
import qualified Data.Macaw.Memory.LoadCommon as LC

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

-- what4
import qualified What4.FunctionName as W4

import Grease.Diagnostic
import Grease.Entrypoint (Entrypoint(..), EntrypointLocation(..))
import qualified Grease.Macaw.Load.Diagnostic as Diag
import Grease.Utility

-- Helper, not exported
doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (LoadDiagnostic diag)

data LoadedProgram arch
  = LoadedProgram
    { progDataLayout :: DataLayout
    , progLoadedBinary :: Loader.LoadedBinary arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
    , progLoadOptions :: LC.LoadOptions
    , progSymMap :: Map.Map (MC.ArchSegmentOff arch) BS.ByteString
      -- ^ A map of all function addresses to their symbol names. Note that it
      -- is possible for a single function address to have multiple function
      -- symbols (https://github.com/GaloisInc/macaw-loader/issues/25), so this
      -- map will arbitrarily pick one of the symbol names.
    , progDynFunMap :: Map.Map W4.FunctionName (MC.ArchSegmentOff arch)
      -- ^ A map of visible, dynamic function symbol names (UTF-8–encoded) to
      -- their corresponding function addresses.
    , progEntrypointAddrs :: Map Entrypoint (MC.ArchSegmentOff arch)
      -- ^ The entrypoint addresses after resolving the user-supplied
      -- 'Entrypoint'.
    }

-- | Compute the 'LC.LoadOptions' for the binary being analyzed. Note that we do
-- different things depending on whether the binary is a position-independent
-- executable (PIE) or not:
--
-- * If the binary is PIE, then we load all addresses relative to a large
--   constant offset. This is to ensure that if @grease@ attempts to dereference
--   a null pointer (i.e., the address 0x0), then the large offset will ensure
--   that valid memory will not be mapped to the address 0x0.
--
-- * If the binary is not PIE, then we load all addresses to exactly the same
--   locations. In general, it would not be correct to load non-PIE addresses
--   relative to a constant offset, as this would break any code that relies on
--   absolute addressing.
loadOptions ::
  -- | 'True' if the loaded binary is a position-independent executable.
  Bool ->
  LC.LoadOptions
loadOptions pie =
  LC.LoadOptions
    { LC.loadOffset = if pie then Just 0x10000000 else Nothing
    }

load ::
  forall arch.
  ( 16 C.<= MC.ArchAddrWidth arch
  , Symbolic.SymArchConstraints arch
  , Loader.BinaryLoader arch (Elf.ElfHeaderInfo (MC.RegAddrWidth (MC.ArchReg arch)))
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  [Entrypoint] ->
  Permissions ->
  Elf.ElfHeaderInfo (MC.RegAddrWidth (MC.ArchReg arch)) ->
  IO (LoadedProgram arch)
load la userEntrypoints perms elf = do
  let loadOpts = loadOptions (Elf.elfIsPie perms elf)
  loaded <- Loader.loadBinary @arch loadOpts elf
  let mem = Loader.memoryImage loaded
  when (Elf.headerType (Elf.header elf) == Elf.ET_REL) $
    throw $ GreaseException "Loading object files is not supported"
  entrypoints <- NE.toList <$> Loader.entryPoints loaded
  let entrypointNamePairs =
        -- Use `mapMaybe` here, because `symbolFor` may return `Nothing` if an
        -- entry point lacks a corresponding function symbol. This is common
        -- with stripped binaries (see gitlab#110).
        mapMaybe
          (\entrypoint -> do
            entrypointName <- Loader.symbolFor loaded (EL.segoffAddr entrypoint)
            pure (entrypoint, entrypointName))
          entrypoints
  let symMap = Map.fromList entrypointNamePairs
  let revSymMap = Map.fromList . fmap Tuple.swap $ Map.toList (Text.decodeUtf8 <$> symMap)
  entryAddrs <-
    forM userEntrypoints $ \entry ->
      case entrypointLocation entry of
        EntrypointSymbolName nm ->
          case Map.lookup nm revSymMap of
            Nothing -> throw . GreaseException $ "Could not find entrypoint symbol " <> tshow nm
            Just addr -> pure (entry, addr)
        EntrypointAddress (readMaybe . Text.unpack -> Just addr)
          | Just so <-
              let offset = fromMaybe 0 (LC.loadOffset loadOpts) in
              let addrWithOffset = EL.memWord (addr + offset) in
              Loader.resolveAbsoluteAddress mem addrWithOffset -> pure (entry, so)
        EntrypointAddress addr -> throw . GreaseException $ "Unable to resolve specified entrypoint address: " <> addr
        EntrypointCoreDump coreDumpPath ->
          (entry,) <$>
          resolveCoreDumpEntrypointAddress la loadOpts mem elf symMap coreDumpPath
  let entryAddrMap = Map.fromList entryAddrs
  let dl = Mem.defaultDataLayout

  let dynFuns = dynamicFunAddrs loadOpts elf
  dynFunSegOffs <-
    traverse
      (traverse
        (traverse
          (\addrWord ->
            case Loader.resolveAbsoluteAddress mem addrWord of
              Just addrSegOff ->
                pure addrSegOff
              Nothing ->
                throw $ GreaseException $
                  "Unable to resolve dynamic function address: " <>
                  tshow addrWord)))
      dynFuns
  let dynFunMap = Tuple.snd <$>
                  List.foldl'
                    (\m (funSym, (entry, addr)) ->
                      -- Load dynamic function symbols, prioritizing symbols
                      -- encountered earlier over later symbols. See
                      -- Note [Weak symbols].
                      addSymbolWithPriority funSym entry addr m)
                    Map.empty dynFunSegOffs

  return
    LoadedProgram
    { progDataLayout = dl
    , progLoadedBinary = loaded
    , progLoadOptions = loadOpts
    , progSymMap = symMap
    , progDynFunMap = dynFunMap
    , progEntrypointAddrs = entryAddrMap
    }

-- Helper, not exported
--
-- Retrieve the UTF-8–encoded names and addresses of all dynamic functions in an
-- ELF binary.
dynamicFunAddrs ::
  forall w.
  MM.MemWidth w =>
  LC.LoadOptions ->
  Elf.ElfHeaderInfo w ->
  [(W4.FunctionName, (Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w), MM.MemWord w))]
dynamicFunAddrs loadOpts ehi =
  case Elf.decodeHeaderDynsym ehi of
    Just (Right symtab) ->
      Elf.elfClassInstances (Elf.headerClass (Elf.header ehi)) (funAddrs symtab)
    _ -> []
  where
    offset = fromMaybe 0 $ LC.loadOffset loadOpts

    funAddrs ::
      Integral (Elf.ElfWordType w) =>
      Elf.Symtab w ->
      [(W4.FunctionName, (Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w), MM.MemWord w))]
    funAddrs symtab =
        Elf.symtabEntries symtab
      & Vec.filter isFuncSymbol
      & Vec.map (\entry -> ( functionNameFromByteString $ Elf.steName entry
                         , (entry, fromIntegral (Elf.steValue entry) + fromIntegral offset)
                         ))
      & Vec.toList

-- Helper, not exported
--
-- Return true if this symbol table entry corresponds to a
-- visibly defined function.
isFuncSymbol :: Integral w => Elf.SymtabEntry nm w -> Bool
isFuncSymbol e
  =  Elf.steType e == Elf.STT_FUNC
  && toInteger (Elf.steValue e) /= 0
  && isVisibleSymbol e

-- Helper, not exported
--
-- Return true if this symbol table entry corresponds to a
-- visibly defined symbol.
isVisibleSymbol :: Elf.SymtabEntry nm w -> Bool
isVisibleSymbol e
  =  -- We consider weak symbols to be visible. See Note [Weak symbols].
     (Elf.steBind e == Elf.STB_GLOBAL || Elf.steBind e == Elf.STB_WEAK)
  && Elf.steIndex e /= Elf.SHN_UNDEF
  && Elf.steIndex e <  Elf.SHN_LORESERVE

-- Helper, not exported
--
-- Add a new @symbol@ (which has been derived from a 'Elf.SymtabEntry' in some
-- way) and associated 'MM.MemSegmentOff' value to a 'Map.Map'. If the 'Map.Map'
-- already contains that @symbol@, the conflict is resolved as follows:
--
-- 1. Global symbols are favored over weak symbols. See @Note [Weak symbols]@.
--    The only reason the 'Map.Map' includes a 'Elf.SymtabEntry' in its range is
--    because we need to consult its 'Elf.steBind' during this step.
--
-- 2. Otherwise, favor the already-encountered @symbol@ over the new @symbol@.
--    This is what implements the lookup scope scheme described in
--    @Note [Dynamic lookup scope]@ in
--    "Data.Macaw.Memory.ElfLoader.DynamicDependencies" in @macaw-base@.
addSymbolWithPriority ::
  Ord symbol =>
  symbol ->
  Elf.SymtabEntry nm (Elf.ElfWordType w) ->
  MM.MemSegmentOff w ->
  Map.Map symbol (Elf.SymtabEntry nm (Elf.ElfWordType w), MM.MemSegmentOff w) ->
  Map.Map symbol (Elf.SymtabEntry nm (Elf.ElfWordType w), MM.MemSegmentOff w)
addSymbolWithPriority newSym newSt newVal =
  Map.insertWith
    (\new@(newSte, _newVal) old@(oldSte, _oldVal) ->
      if -- Step (1)
         |  Elf.steBind oldSte == Elf.STB_GLOBAL
         ,  Elf.steBind newSte == Elf.STB_WEAK
         -> old

         |  Elf.steBind newSte == Elf.STB_GLOBAL
         ,  Elf.steBind oldSte == Elf.STB_WEAK
         -> new

         -- Step (2)
         |  otherwise
         -> old)
    newSym (newSt, newVal)

{-
Note [Weak symbols]
-------------------
Weak symbols are like global symbols, except that a weak symbol is allowed to
be overridden by a global symbol of the same name. Libraries like libc use weak
symbols all over the place. For instance, libc might have a weak symbol named
setuid and a global symbol named __setuid at the same function address. A PLT
stub that jumps to setuid() will likely use the former symbol name, however, so
it's important for grease to be aware of them.

Much of the time, if a weak symbol exists in a binary, then there is no
corresponding global symbol of the same name in the same binary. This is
because the linker usually removes weak symbols of this sort, so by the time
grease simulates the binary, any potential name conflicts between weak and
global symbols have already been resolved. Still, it's difficult to state with
confidence that such a scenario could never happen. Just in case it does, we
manually resolve such naming conflicts (in `addSymbolWithPriority`) by favoring
global symbols over weak symbols in case of a name conflict.

What *does* happen in practice is that an executable can override a weak symbol
in a dependent shared library by defining a global symbol of the same name.
Although grease does not yet support shared libraries that are dynamically
linked from another executable (see #21), it very well could in the future, so
using `addSymbolWithPriority` will make `grease` future-proof in this regard.
-}

-- Helper, not exported
--
-- Given a binary to simulate and a core dump file obtained by running the
-- binary, resolve the address of the function entrypoint closest to the address
-- where the core was dumped. This approach has the following limitations:
--
-- - It is not guaranteed that the function entrypoint that is chosen actually
--   contains the address where the core was dumped. In order to know this for
--   sure, we would have to perform some code discovery here. See #39.
--
-- - Even if the function entrypoint /does/ contain the address where the core
--   was dumped, it is not guaranteed that @grease@ will recreate the conditions
--   that caused the core to be dumped. In order to do so, @grease@ would have
--   to perform further analysis on the core dump file in order to precisely
--   recreate the function arguments and memory state that led to the dumped
--   address being invoked the way it was in the core dump file. See #40.
resolveCoreDumpEntrypointAddress ::
  forall w.
  MM.MemWidth w =>
  GreaseLogAction ->
  LC.LoadOptions ->
  -- | The 'MM.Memory' of the binary to simulate.
  MM.Memory w ->
  -- | The 'Elf.ElfHeaderInfo' of the binary to simulate.
  Elf.ElfHeaderInfo w ->
  -- | A 'Map.Map' of function entrypoint addresses to their names.
  Map.Map (MM.MemSegmentOff w) BS.ByteString ->
  -- | The path to the core dump file, which was obtained by running the binary
  -- to simulate.
  FilePath ->
  -- | The address of the function entrypoint closest to the address where the
  -- core was dumped.
  IO (MM.MemSegmentOff w)
resolveCoreDumpEntrypointAddress la loadOpts mem binaryHeaderInfo symMap coreDumpPath = do
  coreDumpBytes <- BS.readFile coreDumpPath
  withElfHeader coreDumpBytes $ \coreDumpHeaderInfo -> do
    let binaryHeader = Elf.header binaryHeaderInfo
        binaryClass = Elf.headerClass binaryHeader
        coreDumpHeader = Elf.header coreDumpHeaderInfo
        coreDumpClass = Elf.headerClass coreDumpHeader
    C.Refl <-
      case C.testEquality binaryClass coreDumpClass of
        Just r -> pure r
        Nothing -> throw $ GreaseException $ Text.unlines
          [ "The binary and the core dump file have different ELF classes."
          , "Binary:         " <> tshow binaryClass
          , "Core dump file: " <> tshow coreDumpClass
          ]
    Elf.elfClassInstances coreDumpClass $ do
      -- Decode the ELF notes in the core dump file.
      notes <-
        case CoreDump.decodeHeaderNotes coreDumpHeaderInfo of
          Right notes -> pure notes
          Left err -> throw $ GreaseException $ tshow err

      -- Compute the address where the core was dumped in the core dump file.
      prStatusPc <-
        case CoreDump.coreDumpHeaderProgramCounter coreDumpHeaderInfo notes of
          Right prStatusPc -> pure prStatusPc
          Left err -> throw $ GreaseException $ tshow err

      -- Finally, compute the nearest entrypoint address.
      let offset :: Elf.ElfWordType w
          offset = fromIntegral $ fromMaybe 0 $ LC.loadOffset loadOpts

          prStatusPcWithOffset :: MM.MemWord w
          prStatusPcWithOffset = fromIntegral $ prStatusPc + offset
      prStatusPcSegOff <-
        case Loader.resolveAbsoluteAddress mem prStatusPcWithOffset of
          Just prStatusPcSegOff -> pure prStatusPcSegOff
          Nothing -> throw $ GreaseException $
            "The core was dumped at address " <> tshow prStatusPcWithOffset <>
            ", but this could not be resolved to an address in the binary."
      (nearestEntrypointAddr, nearestEntrypointName) <-
        case Map.lookupLE prStatusPcSegOff symMap of
          Just nearestEntrypoint -> pure nearestEntrypoint
          Nothing -> throw $ GreaseException
            "Could not find an entrypoint address in the binary."
      doLog la $
        Diag.DiscoveredCoreDumpEntrypoint
          nearestEntrypointAddr
          (functionNameFromByteString nearestEntrypointName)
          prStatusPcWithOffset
      pure nearestEntrypointAddr

-- Helper, not exported
--
-- Decode an ELF file and pass it to a continuation.
withElfHeader :: BS.ByteString -> (forall w. Elf.ElfHeaderInfo w -> IO r) -> IO r
withElfHeader bs k =
  case Elf.decodeElfHeaderInfo bs of
    Left (_,err) ->
      throw $ GreaseException $ "Failed to parse ELF file: " <> tshow err
    Right (Elf.SomeElf e) -> k e

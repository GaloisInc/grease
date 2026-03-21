{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Load (
  LoadedProgram (..),
  LoadedBinaryInfo (..),
  LoadError (..),
  load,
  dumpSections,
  memSegOffToJson,
  -- * Shared library loading utilities
  indexToLoadOptions,
  addressToIndex,
  loadOffset,
  dynamicFunAddrs,
  addSymbolWithPriority,
  isFuncSymbol,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ElfEdit qualified as Elf
import Data.ElfEdit.CoreDump qualified as CoreDump
import Data.Function ((&))
import Data.List qualified as List
import Data.Macaw.BinaryLoader qualified as Loader
import Data.Macaw.BinaryLoader.ELF qualified as Loader
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.ElfLoader qualified as EL
import Data.Macaw.Memory.ElfLoader.DynamicDependencies qualified as DynDeps
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Tuple qualified as Tuple
import Data.Vector qualified as Vec
import Data.Word (Word64)
import Grease.Diagnostic (Diagnostic (LoadDiagnostic), GreaseLogAction)
import Grease.Entrypoint (Entrypoint, EntrypointLocation (EntrypointAddress, EntrypointCoreDump, EntrypointSymbolName), entrypointLocation)
import Grease.Macaw.Load.Diagnostic qualified as Diag
import Grease.Utility (functionNameFromByteString, tshow)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import System.Directory (Permissions)
import System.FilePath qualified as SF
import Text.Read (readMaybe)
import What4.FunctionName qualified as WFN

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (LoadDiagnostic diag)

-- | Per-binary metadata needed for code discovery and PLT resolution.
-- Each loaded binary (main or shared library) gets one of these.
data LoadedBinaryInfo arch
  = LoadedBinaryInfo
  { lbiLoadedBinary :: Loader.LoadedBinary arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , lbiLoadOptions :: LC.LoadOptions
  , lbiPath :: FilePath
  , lbiSymMap :: Map.Map (MC.ArchSegmentOff arch) BS.ByteString
  -- ^ Map of function addresses to their symbol names for this binary.
  , lbiPltStubs :: Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName
  -- ^ PLT stubs discovered in this binary.
  }

data LoadedProgram arch
  = LoadedProgram
  { progLoadedBinary :: Loader.LoadedBinary arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , progLoadOptions :: LC.LoadOptions
  , progSymMap :: Map.Map (MC.ArchSegmentOff arch) BS.ByteString
  -- ^ A map of all function addresses to their symbol names. Note that it
  -- is possible for a single function address to have multiple function
  -- symbols (https://github.com/GaloisInc/macaw-loader/issues/25), so this
  -- map will arbitrarily pick one of the symbol names.
  , progDynFunMap :: Map.Map WFN.FunctionName (MC.ArchSegmentOff arch)
  -- ^ A map of visible, dynamic function symbol names (UTF-8–encoded) to
  -- their corresponding function addresses. When shared libraries are
  -- loaded, this includes symbols from all binaries merged in global
  -- lookup scope order. See @Note [Dynamic lookup scope]@ in
  -- "Data.Macaw.Memory.ElfLoader.DynamicDependencies".
  , progEntrypointAddrs :: Map Entrypoint (MC.ArchSegmentOff arch)
  -- ^ The entrypoint addresses after resolving the user-supplied
  -- 'Entrypoint'.
  , progSharedLibraries :: Vec.Vector (LoadedBinaryInfo arch)
  -- ^ Shared libraries in global lookup scope order.
  -- Empty if shared library simulation is not enabled.
  -- See @Note [Address offsets for shared libraries]@.
  , progSharedLibStartIndex :: Word64
  -- ^ The address-space index of the first shared library. Use
  -- @addressToIndex addr - progSharedLibStartIndex@ to look up the
  -- correct entry in 'progSharedLibraries'.
  }

-- | Errors that can occur during binary loading
--
-- See the 'PP.Pretty' instance for details on each constructor.
data LoadError
  = UnsupportedObjectFile
  | EntrypointNotFound Text.Text
  | InvalidEntrypointAddress Text.Text
  | DynamicFunctionAddressUnresolvable Word64
  | ElfParseError Text.Text
  | CoreDumpClassMismatch Text.Text Text.Text
  | CoreDumpNotesError CoreDump.NoteDecodeError
  | CoreDumpPcError CoreDump.CoreDumpAnalyzeError
  | CoreDumpAddressUnresolvable Word64
  | CoreDumpNoEntrypoint

instance PP.Pretty LoadError where
  pretty =
    \case
      UnsupportedObjectFile ->
        "Loading object files is not supported"
      EntrypointNotFound nm ->
        "Could not find entrypoint symbol" PP.<+> PP.dquotes (PP.pretty nm)
      InvalidEntrypointAddress addr ->
        "Unable to resolve specified entrypoint address:" PP.<+> PP.pretty addr
      DynamicFunctionAddressUnresolvable addr ->
        "Unable to resolve dynamic function address:" PP.<+> PP.pretty addr
      ElfParseError err ->
        "Failed to parse ELF file:" PP.<+> PP.pretty err
      CoreDumpClassMismatch binaryClass coreDumpClass ->
        PP.vsep
          [ "The binary and the core dump file have different ELF classes."
          , "Binary:" PP.<+> PP.pretty binaryClass
          , "Core dump file:" PP.<+> PP.pretty coreDumpClass
          ]
      CoreDumpNotesError err ->
        "Error decoding ELF notes in core dump file:" PP.<+> PP.viaShow err
      CoreDumpPcError err ->
        "Error computing program counter from core dump:" PP.<+> PP.viaShow err
      CoreDumpAddressUnresolvable addr ->
        "The core was dumped at address"
          PP.<+> PP.pretty addr
          PP.<+> ", but this could not be resolved to an address in the binary."
      CoreDumpNoEntrypoint ->
        "Could not find an entrypoint address in the binary."

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
  , ?memOpts :: CLM.MemOptions
  ) =>
  GreaseLogAction ->
  [Entrypoint] ->
  Permissions ->
  FilePath ->
  -- ^ Path to the binary being loaded (used for shared library search)
  Bool ->
  -- ^ Whether to simulate shared libraries
  [FilePath] ->
  -- ^ Directories to search for shared libraries
  [FilePath] ->
  -- ^ Explicit paths to shared libraries to load
  Elf.ElfHeaderInfo (MC.RegAddrWidth (MC.ArchReg arch)) ->
  IO (Either LoadError (LoadedProgram arch))
load la userEntrypoints perms progPath simulateSharedLibs sharedLibDirs explicitSharedLibs elf = runExceptT $ do
  let loadOpts = loadOptions (Elf.elfIsPie perms elf)
  loaded <- liftIO $ Loader.loadBinary @arch loadOpts elf
  let mem = Loader.memoryImage loaded
  when (Elf.headerType (Elf.header elf) == Elf.ET_REL) $
    throwE UnsupportedObjectFile
  entrypoints <- liftIO $ Loader.entryPoints loaded
  let entrypointNamePairs =
        -- Use `mapMaybe` here, because `symbolFor` may return `Nothing` if an
        -- entry point lacks a corresponding function symbol. This is common
        -- with stripped binaries (see gitlab#110).
        mapMaybe
          ( \entrypoint -> do
              entrypointName <- Loader.symbolFor loaded (EL.segoffAddr entrypoint)
              pure (entrypoint, entrypointName)
          )
          entrypoints
  let symMap = Map.fromList entrypointNamePairs
  let revSymMap = Map.fromList . fmap Tuple.swap $ Map.toList (Text.decodeUtf8 <$> symMap)
  entryAddrs <-
    forM userEntrypoints $ \entry ->
      case entrypointLocation entry of
        EntrypointSymbolName nm ->
          case Map.lookup nm revSymMap of
            Nothing -> throwE $ EntrypointNotFound nm
            Just addr -> pure (entry, addr)
        EntrypointAddress (readMaybe . Text.unpack -> Just addr)
          | Just so <-
              let offset = fromMaybe 0 (LC.loadOffset loadOpts)
               in let addrWithOffset = EL.memWord (addr + offset)
                   in Loader.resolveAbsoluteAddress mem addrWithOffset ->
              pure (entry, so)
        EntrypointAddress addr -> throwE $ InvalidEntrypointAddress addr
        EntrypointCoreDump coreDumpPath ->
          (entry,)
            <$> resolveCoreDumpEntrypointAddress la loadOpts mem elf symMap coreDumpPath
  let entryAddrMap = Map.fromList entryAddrs

  let dynFuns = dynamicFunAddrs loadOpts elf
  dynFunSegOffs <-
    traverse
      ( traverse
          ( traverse
              ( \addrWord ->
                  case Loader.resolveAbsoluteAddress mem addrWord of
                    Just addrSegOff ->
                      pure addrSegOff
                    Nothing ->
                      let addr = MM.memWordValue addrWord
                       in throwE (DynamicFunctionAddressUnresolvable addr)
              )
          )
      )
      dynFuns
  -- Build the initial dynamic function map from the main binary
  let mainDynFunMapWithPriority =
        List.foldl'
          ( \m (funSym, (entry, addr)) ->
              addSymbolWithPriority funSym entry addr m
          )
          Map.empty
          dynFunSegOffs

  -- Load shared libraries if enabled (DT_NEEDED + explicit --shared-lib paths)
  (soInfos, soStartIdx) <-
    if simulateSharedLibs || not (null explicitSharedLibs)
      then liftIO $ loadSharedLibraries la progPath loadOpts sharedLibDirs explicitSharedLibs simulateSharedLibs elf
      else do
        doLog la Diag.SharedLibraryLoadingDisabled
        pure (Vec.empty, 1)

  -- Merge dynamic function symbols from all shared libraries (in global
  -- lookup scope order) into the main binary's dynFunMap.
  -- See Note [Dynamic lookup scope] in DynamicDependencies.
  mergedDynFunMapWithPriority <- do
    let addSoSymbols accMap soInfo = do
          let soMem = Loader.memoryImage (lbiLoadedBinary soInfo)
          let soLoadOpts = lbiLoadOptions soInfo
          let soEhi = Loader.originalBinary (lbiLoadedBinary soInfo)
          let soDynFuns = dynamicFunAddrs soLoadOpts soEhi
          soDynFunSegOffs <-
            traverse
              ( traverse
                  ( traverse
                      ( \addrWord ->
                          case Loader.resolveAbsoluteAddress soMem addrWord of
                            Just addrSegOff -> pure addrSegOff
                            Nothing ->
                              let addr = MM.memWordValue addrWord
                               in throwE (DynamicFunctionAddressUnresolvable addr)
                      )
                  )
              )
              soDynFuns
          pure $
            List.foldl'
              ( \m (funSym, (entry, addr)) ->
                  addSymbolWithPriority funSym entry addr m
              )
              accMap
              soDynFunSegOffs
    Vec.foldM' addSoSymbols mainDynFunMapWithPriority soInfos

  let dynFunMap = Tuple.snd <$> mergedDynFunMapWithPriority

  -- Merge symMaps from shared libraries
  let mergedSymMap =
        Vec.foldl'
          (\acc soInfo -> Map.union acc (lbiSymMap soInfo))
          symMap
          soInfos

  return
    LoadedProgram
      { progLoadedBinary = loaded
      , progLoadOptions = loadOpts
      , progSymMap = mergedSymMap
      , progDynFunMap = dynFunMap
      , progEntrypointAddrs = entryAddrMap
      , progSharedLibraries = soInfos
      , progSharedLibStartIndex = soStartIdx
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
  [(WFN.FunctionName, (Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w), MM.MemWord w))]
dynamicFunAddrs loadOpts ehi =
  case Elf.decodeHeaderDynsymLenient ehi of
    Right (Just symtab) ->
      Elf.elfClassInstances (Elf.headerClass (Elf.header ehi)) (funAddrs symtab)
    _ -> []
 where
  offset = fromMaybe 0 $ LC.loadOffset loadOpts

  funAddrs ::
    Integral (Elf.ElfWordType w) =>
    Elf.Symtab w ->
    [(WFN.FunctionName, (Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w), MM.MemWord w))]
  funAddrs symtab =
    Elf.symtabEntries symtab
      & Vec.filter isFuncSymbol
      & Vec.map
        ( \entry ->
            ( functionNameFromByteString $ Elf.steName entry
            , (entry, fromIntegral (Elf.steValue entry) + fromIntegral offset)
            )
        )
      & Vec.toList

-- Helper, not exported
--
-- Return true if this symbol table entry corresponds to a
-- visibly defined function.
isFuncSymbol :: Integral w => Elf.SymtabEntry nm w -> Bool
isFuncSymbol e =
  Elf.steType e == Elf.STT_FUNC
    && toInteger (Elf.steValue e) /= 0
    && isVisibleSymbol e

-- Helper, not exported
--
-- Return true if this symbol table entry corresponds to a
-- visibly defined symbol.
isVisibleSymbol :: Elf.SymtabEntry nm w -> Bool
isVisibleSymbol e =
  -- We consider weak symbols to be visible. See Note [Weak symbols].
  (Elf.steBind e == Elf.STB_GLOBAL || Elf.steBind e == Elf.STB_WEAK)
    && Elf.steIndex e /= Elf.SHN_UNDEF
    && Elf.steIndex e < Elf.SHN_LORESERVE

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
    ( \new@(newSte, _newVal) old@(oldSte, _oldVal) ->
        if
          -- Step (1)
          | Elf.steBind oldSte == Elf.STB_GLOBAL
          , Elf.steBind newSte == Elf.STB_WEAK ->
              old
          | Elf.steBind newSte == Elf.STB_GLOBAL
          , Elf.steBind oldSte == Elf.STB_WEAK ->
              new
          -- Step (2)
          | otherwise ->
              old
    )
    newSym
    (newSt, newVal)

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
  ExceptT LoadError IO (MM.MemSegmentOff w)
resolveCoreDumpEntrypointAddress la loadOpts mem binaryHeaderInfo symMap coreDumpPath = do
  coreDumpBytes <- liftIO $ BS.readFile coreDumpPath
  withElfHeader coreDumpBytes $ \coreDumpHeaderInfo -> do
    let binaryHeader = Elf.header binaryHeaderInfo
        binaryClass = Elf.headerClass binaryHeader
        coreDumpHeader = Elf.header coreDumpHeaderInfo
        coreDumpClass = Elf.headerClass coreDumpHeader
    C.Refl <-
      case C.testEquality binaryClass coreDumpClass of
        Just r -> pure r
        Nothing ->
          throwE $
            CoreDumpClassMismatch (tshow binaryClass) (tshow coreDumpClass)
    Elf.elfClassInstances coreDumpClass $ do
      -- Decode the ELF notes in the core dump file.
      notes <-
        case CoreDump.decodeHeaderNotes coreDumpHeaderInfo of
          Right notes -> pure notes
          Left err -> throwE $ CoreDumpNotesError err

      -- Compute the address where the core was dumped in the core dump file.
      prStatusPc <-
        case CoreDump.coreDumpHeaderProgramCounter coreDumpHeaderInfo notes of
          Right prStatusPc -> pure prStatusPc
          Left err -> throwE $ CoreDumpPcError err

      -- Finally, compute the nearest entrypoint address.
      let offset :: Elf.ElfWordType w
          offset = fromIntegral $ fromMaybe 0 $ LC.loadOffset loadOpts

          prStatusPcWithOffset :: MM.MemWord w
          prStatusPcWithOffset = fromIntegral $ prStatusPc + offset
      prStatusPcSegOff <-
        case Loader.resolveAbsoluteAddress mem prStatusPcWithOffset of
          Just prStatusPcSegOff -> pure prStatusPcSegOff
          Nothing ->
            throwE $
              CoreDumpAddressUnresolvable (MM.memWordValue prStatusPcWithOffset)
      (nearestEntrypointAddr, nearestEntrypointName) <-
        case Map.lookupLE prStatusPcSegOff symMap of
          Just nearestEntrypoint -> pure nearestEntrypoint
          Nothing ->
            throwE CoreDumpNoEntrypoint
      liftIO $
        doLog la $
          Diag.DiscoveredCoreDumpEntrypoint
            nearestEntrypointAddr
            (functionNameFromByteString nearestEntrypointName)
            prStatusPcWithOffset
      pure nearestEntrypointAddr

memSegOffToJson :: MM.MemWidth w => MM.MemSegmentOff w -> Aeson.Value
memSegOffToJson memSegOff =
  let addr = MM.segoffAddr memSegOff
   in Aeson.Object $
        KeyMap.fromList
          [ ("region_index", Aeson.toJSON $ MM.addrBase addr)
          , ("region_offset", Aeson.toJSON $ fromIntegral @_ @Word64 $ MM.addrOffset addr)
          ]

-- | Dumps the section map of memory that is a map from SectionIndex->memSegOff in a
-- format that can be rendered as JSON. Each section index is mapped to a json representation of a
-- 'MM.MemAddr' that contains the base region of the address along with the offset.
-- By keeping addresses relative to the section index the representation of addresses (e.g. for coverage)
-- is relocatable for clients that load a PIE at a different base address.
dumpSections :: MM.MemWidth w => MM.Memory w -> Aeson.Value
dumpSections mem =
  let
    secs = MM.memSectionIndexMap mem
    sinfo =
      map
        ( \(secIdx, memSegOff) ->
            let seg = memSegOffToJson memSegOff
                secIdxInt :: Int
                secIdxInt = fromIntegral secIdx
             in Aeson.Object $
                  KeyMap.fromList
                    [ ("section_index", Aeson.toJSON secIdxInt)
                    , ("section_mem_addr", seg)
                    ]
        )
        (Map.toList secs)
   in
    Aeson.toJSON sinfo

-- Helper, not exported
--
-- Decode an ELF file and pass it to a continuation.
withElfHeader :: BS.ByteString -> (forall w. Elf.ElfHeaderInfo w -> ExceptT LoadError IO r) -> ExceptT LoadError IO r
withElfHeader bs k =
  case Elf.decodeElfHeaderInfo bs of
    Left (_, err) ->
      throwE $ ElfParseError (Text.pack err)
    Right (Elf.SomeElf e) -> k e

-- | The address offset multiplier used for shared library address space
-- partitioning. Each shared library is loaded at @loadOffset * i@ where @i@
-- is the 1-based index in BFS order.
--
-- See @Note [Address offsets for shared libraries]@.
loadOffset :: Num a => a
loadOffset = 0x10000000

-- | Given an index value, constructs a 'LC.LoadOptions' for the appropriate
-- offset to load a shared object at.
-- See @Note [Address offsets for shared libraries]@.
indexToLoadOptions :: Word64 -> LC.LoadOptions
indexToLoadOptions index =
  LC.LoadOptions { LC.loadOffset = Just $ loadOffset * index }

-- | Given an address, determine the index of the binary that defines
-- the address. Index 0 is the main binary, 1 is the first shared library, etc.
--
-- See @Note [Address offsets for shared libraries]@.
addressToIndex :: MM.MemWord w -> Integer
addressToIndex addr = MM.memWordToUnsigned addr `Bits.shiftR` 28

{-
Note [Address offsets for shared libraries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When loading a binary with shared library dependencies, we must ensure that the
absolute addresses for each binary do not clash with each other. We accomplish
this by using the following conventions for address offsets when loading
binaries:

* Addresses in the main binary are loaded as-is.
* For shared libraries lib_1, lib_2, ..., lib_n, the addresses in lib_i are
  offset by @0x10000000 * i@. (See 'indexToLoadOptions'.)

When resolving addresses, we must also go in the opposite direction. That is,
we must be able to take an address with an already-applied offset and determine
which binary it originally came from. To do so:

* The 'addressToIndex' function masks off the low bits in the address to
  determine the index of the binary. For instance, given the address
  0x20001000, its high bits are 2, indicating that the address comes from the
  second shared library. If the index is 0, then the address comes from the
  main binary.
* The 'progSharedLibraries' field in 'LoadedProgram' contains a vector of each
  shared library's 'LoadedBinaryInfo' at an index suitable for use with the
  indexing conventions established above (shifted by 1, since index 0 is the
  main binary).

Be aware of the caveats to this approach to offsets:

* This load offset calculation is safe so long as all binaries are
  under 256MB. It seems likely that something else in the verifier would
  fail before binaries reach that size.

* On 32-bit architectures, index values of 16 or higher will cause
  the offset to reach inaccessible values. Macaw throws an exception in
  this case. If this occurs in practice we will need to reduce the offset
  multiplier to something smaller (the trade-off is that the maximum
  allowable library size will also decrease).

* This approach requires some care regarding PLT stubs, as they add a layer of
  indirection between PLT function addresses, which reside in one binary, and
  the addresses that they ultimately jump to, which reside in a different
  binary. For this reason, PLT stubs are handled as a special case in function
  lookup that bypasses the indexing mechanisms described above.
-}

-- | Load dynamic dependencies of a binary when shared library simulation is
-- enabled. Returns 'LoadedBinaryInfo' for each shared library, and the
-- merged dynamic function symbol map across all binaries.
loadSharedLibraries ::
  forall arch.
  ( 16 C.<= MC.ArchAddrWidth arch
  , Symbolic.SymArchConstraints arch
  , Loader.BinaryLoader arch (Elf.ElfHeaderInfo (MC.RegAddrWidth (MC.ArchReg arch)))
  , ?memOpts :: CLM.MemOptions
  ) =>
  GreaseLogAction ->
  FilePath ->
  -- ^ The main binary's path (used as a fallback search directory)
  LC.LoadOptions ->
  -- ^ The main binary's load options (needed to compute SO index offsets;
  -- if the main binary is PIE and loaded at @0x10000000@, SOs must start
  -- at index 2 to avoid address space collisions)
  [FilePath] ->
  -- ^ Directories to search for shared libraries. The first directory in
  -- the list is used (macaw's 'loadDynamicDependencies' currently only
  -- supports a single search directory). If empty, falls back to the
  -- binary's directory.
  [FilePath] ->
  -- ^ Explicit paths to shared libraries (from @--shared-lib@)
  Bool ->
  -- ^ Whether to load DT_NEEDED dependencies (False when @--no-shared-libs@
  -- is used but explicit @--shared-lib@ paths are still provided)
  Elf.ElfHeaderInfo (MC.RegAddrWidth (MC.ArchReg arch)) ->
  -- ^ The main binary's ELF header info
  IO (Vec.Vector (LoadedBinaryInfo arch), Word64)
  -- ^ (shared library infos, starting address-space index)
loadSharedLibraries la mainBinPath mainLoadOpts sharedLibDirs explicitLibs loadDtNeeded mainEhi = do
  let hdr = Elf.header mainEhi
  let hdrMachine = Elf.headerMachine hdr
  let hdrClass = Elf.headerClass hdr
  -- Use the first search directory, falling back to the binary's directory
  let searchDir =
        case sharedLibDirs of
          (d:_) -> d
          [] -> SF.takeDirectory mainBinPath

  -- BFS traversal of DT_NEEDED entries (only if enabled)
  dtNeededSos <-
    if loadDtNeeded
      then do
        foundSos <- DynDeps.loadDynamicDependencies hdrMachine hdrClass searchDir mainEhi mainBinPath
        -- Log any DT_NEEDED entries that weren't found
        let foundNames = map snd foundSos
        case DynDeps.parseDynNeeded mainEhi mainBinPath of
          Just (Right neededNames) -> do
            let notFound = filter (\n -> BSC.unpack n `notElem` foundNames) neededNames
            forM_ notFound $ \name ->
              doLog la $ Diag.SharedLibraryNotFound (BSC.unpack name)
          _ -> pure ()
        pure foundSos
      else pure []

  -- Load explicit --shared-lib paths
  explicitSos <- forM explicitLibs $ \libPath -> do
    soBytes <- BS.readFile libPath
    case Elf.decodeElfHeaderInfo soBytes of
      Right (Elf.SomeElf soEhi) ->
        case C.testEquality (Elf.headerClass (Elf.header soEhi)) hdrClass of
          Just C.Refl -> pure (soEhi, libPath)
          Nothing -> error $ "Shared library " ++ libPath ++ " has different ELF class than main binary"
      Left (_, err) -> error $ "Failed to parse shared library " ++ libPath ++ ": " ++ err

  -- Combine DT_NEEDED and explicit SOs (explicit ones come after DT_NEEDED
  -- in lookup scope order)
  let sos = dtNeededSos ++ explicitSos

  -- Compute the starting SO index. If the main binary is loaded at an offset
  -- (e.g., PIE at 0x10000000), the first SO must use a higher index to avoid
  -- address space collisions.
  let mainOffset = fromMaybe 0 (LC.loadOffset mainLoadOpts)
  let startIdx = if mainOffset > 0
        then mainOffset `div` loadOffset + 1
        else 1

  -- Load each shared library with appropriate address offset
  soInfos <- forM (zip [startIdx..] sos) $ \(idx :: Word64, (soEhi, soPath)) -> do
    let soLoadOpts = indexToLoadOptions idx
    soLoaded <- Loader.loadBinary @arch soLoadOpts soEhi
    entrypoints <- Loader.entryPoints soLoaded
    let soSymMap =
          Map.fromList $
            mapMaybe
              ( \entrypoint -> do
                  entrypointName <- Loader.symbolFor soLoaded (EL.segoffAddr entrypoint)
                  pure (entrypoint, entrypointName)
              )
              entrypoints

    doLog la $ Diag.LoadedSharedLibrary soPath idx

    pure LoadedBinaryInfo
      { lbiLoadedBinary = soLoaded
      , lbiLoadOptions = soLoadOpts
      , lbiPath = soPath
      , lbiSymMap = soSymMap
      , lbiPltStubs = Map.empty  -- PLT stubs are computed later in Main.hs
      }

  pure (Vec.fromList soInfos, startIdx)

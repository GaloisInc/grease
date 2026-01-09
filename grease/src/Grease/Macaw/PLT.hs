{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | See @doc/shared-libraries.md@ for user-facing documentation about PLT
-- stubs.
--
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.PLT (
  pltStubSymbols,
  PltStub (..),
  PltStubParser,
  pltStubParser,
  CouldNotResolvePltStub (..),
  resolvePltStubs,
) where

import Control.Applicative (empty)
import Data.ElfEdit qualified as Elf
import Data.Foldable qualified as Foldable
import Data.Macaw.BinaryLoader.ELF as Loader
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.ElfLoader.PLTStubs qualified as PLT
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)
import Grease.Panic (panic)
import Grease.Utility
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.FunctionName qualified as WFN

-- | A variant of 'PLT.pltStubSymbols' that performs some experimental hacks to
-- obtain more reliable results. In particular:
--
-- * This function explicitly filters out any PLT stub addresses that do not
--   reside within the @.plt@ section. This is a crude workaround for
--   <https://github.com/GaloisInc/macaw/issues/375>, which can result in
--   inaccurate @.plt.got@ identification. See also @Note [Subtleties of
--   resolving PLT stubs] (Wrinkle 1: .plt.got)@.
--
--   This is far from a perfect solution, as it means that @grease@ will simply
--   fail to identify any PLT stubs that reside within a non-@.plt@ section. As
--   such, we rely on the user to supply any missing PLT stubs via the
--   @--plt-stub@ command-line flag.
--
-- * If the binary contains a @.plt.sec@ section, it adjusts the results of
--   'PLT.pltStubSymbols' to refer to stubs in the @.plt.sec@ section instead of
--   the @.plt@ section. See also @Note [Subtleties of resolving PLT stubs]
--   (Wrinkle 2: .plt.sec)@.
pltStubSymbols ::
  forall reloc w.
  ( w ~ Elf.RelocationWidth reloc
  , MM.MemWidth w
  , Elf.IsRelocationType reloc
  ) =>
  -- | Heuristics about how large PLT stubs should be.
  PLT.PLTStubInfo reloc ->
  -- | Options configuring how to load the address of each PLT stub.
  LC.LoadOptions ->
  -- | The dynamically linked ELF binary.
  Elf.ElfHeaderInfo w ->
  Map.Map (MM.MemWord w) (Elf.VersionedSymbol (Elf.ElfWordType w))
pltStubSymbols pltStubInfo loadOptions ehi =
  Elf.elfClassInstances elfClass $
    case listToMaybe (Elf.findSectionByName ".plt.sec" elf) of
      -- We have a .plt.sec section, so compute the addresses of the .plt stubs
      -- and fix up their addresses afterwards.
      Just pltSec ->
        let
          -- The base addresses of the .plt and .plt.sec sections, respectively.
          pltBase, pltSecBase :: Elf.ElfWordType w
          pltBase =
            case listToMaybe (Elf.findSectionByName ".plt" elf) of
              Just plt ->
                Elf.elfSectionAddr plt + loadOffset
              Nothing ->
                panic "pltStubSymbols" [".plt.sec section without .plt section"]
          pltSecBase = Elf.elfSectionAddr pltSec + loadOffset

          -- Compute the constant offset that we need to add to each .plt stub
          -- address in order to make them valid .plt.sec stub addresses. See
          -- Note [Subtleties of resolving PLT stubs] (Wrinkle 2: .plt.sec).
          pltOffset :: MM.MemWord w
          pltOffset =
            fromIntegral (pltSecBase - pltBase)
              - fromInteger (PLT.pltFunSize pltStubInfo)
         in
          Map.mapKeys (+ pltOffset) pltStubs
      -- We do not have a .plt.sec section, so simply compute the addresses of the
      -- .plt stubs.
      Nothing ->
        pltStubs
 where
  (_, elf) = Elf.getElf ehi
  elfClass = Elf.elfClass elf

  -- The stubs from the .plt section.
  pltStubs ::
    (Ord (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
    Map.Map (MM.MemWord w) (Elf.VersionedSymbol (Elf.ElfWordType w))
  pltStubs =
    Map.filterWithKey
      (\addr _ -> withinPltSection addr)
      (PLT.pltStubSymbols pltStubInfo loadOptions ehi)

  withinPltSection ::
    (Ord (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
    MM.MemWord w ->
    Bool
  withinPltSection addr =
    case Elf.findSectionByName ".plt" elf of
      [section] -> withinSection addr section
      _ -> False

  withinSection ::
    (Ord (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
    MM.MemWord w ->
    Elf.ElfSection (Elf.ElfWordType w) ->
    Bool
  withinSection addr section =
    let addr' :: Elf.ElfWordType w
        addr' = fromIntegral addr

        secBegin = Elf.elfSectionAddr section + loadOffset
        secEnd = secBegin + Elf.elfSectionSize section
     in secBegin <= addr' && addr' < secEnd

  loadOffset :: Num (Elf.ElfWordType w) => Elf.ElfWordType w
  loadOffset = fromIntegral $ fromMaybe 0 (LC.loadOffset loadOptions)

-- | A PLT stub address and name.
data PltStub
  = PltStub
      -- | The stub's address.
      Word64
      -- | The stub's name.
      Text
  deriving Show

-- | A @megaparsec@ parser type for 'PltStub's.
type PltStubParser = TM.Parsec Void Text

-- | Parse a symbol from 'TM.Tokens'.
symbol :: TM.Tokens Text -> PltStubParser Text
symbol = TMCL.symbol spaceConsumer

-- | A standard space consumer that does not support comments.
spaceConsumer :: PltStubParser ()
spaceConsumer = TMCL.space TMC.space1 empty empty

-- | Parse a 'PltStub' in the format @ADDRESS:NAME@, where @ADDRESS@ is a
-- hexadecimal number and @NAME@ is an arbitrary symbol.
pltStubParser :: PltStubParser PltStub
pltStubParser = do
  addr <- symbol "0x" *> TMCL.hexadecimal
  _ <- TMC.char ':'
  name <- TM.takeWhileP (Just "name") (/= ':')
  pure $ PltStub addr name

data CouldNotResolvePltStub
  = CouldNotResolvePltStub FilePath Word64 WFN.FunctionName

instance PP.Pretty CouldNotResolvePltStub where
  pretty (CouldNotResolvePltStub path addr name) =
    "Could not resolve PLT stub address "
      <> PP.viaShow addr
      <> ": "
      <> PP.pretty (WFN.functionName name)
      <> " in binary "
      <> PP.pretty path

-- | Resolve all PLT stubs in a binary, consulting:
--
-- * The the @.plt@ or @.plt.sec@ section (see the 'pltStubSymbols' function)
--
-- * @SymbolReloc@ dynamic relocations
--
-- * User-provided 'PltStub's
--
-- See @Note [Subtleties of resolving PLT stubs]@.
resolvePltStubs ::
  forall reloc w.
  ( w ~ Elf.RelocationWidth reloc
  , MM.MemWidth w
  , Elf.IsRelocationType reloc
  ) =>
  -- | Path to the binary, used in exception messages
  FilePath ->
  -- | Heuristics about how large PLT stubs should be. If the binary uses an
  --          architecture for which @grease@ cannot find PLT stubs (see
  --          @Note [Subtleties of resolving PLT stubs] (Wrinkle 3: PowerPC)@ for one
  --          such example), then this will be 'Nothing'.
  Maybe (PLT.PLTStubInfo reloc) ->
  -- | Options configuring how to load the address of each PLT stub.
  LC.LoadOptions ->
  -- | The dynamically linked ELF binary.
  Elf.ElfHeaderInfo w ->
  -- | Map of @SymbolReloc@ relocation addresses to their symbols.
  Map.Map (MM.MemWord w) WFN.FunctionName ->
  -- | User-specified PLT stubs.
  [PltStub] ->
  MC.Memory w ->
  IO (Either CouldNotResolvePltStub (Map.Map (MM.MemSegmentOff w) WFN.FunctionName))
resolvePltStubs path mbPltStubInfo loadOptions ehi symbolRelocs userPltStubs memory = do
  let
    -- This only contains the PLT stubs located in the @.plt@ section, for
    -- which Macaw's heuristics are somewhat reliable.
    pltStubAddrToNameMap :: Map.Map (MM.MemWord w) WFN.FunctionName
    pltStubAddrToNameMap =
      case mbPltStubInfo of
        Just pltStubInfo ->
          Map.map (functionNameFromByteString . Elf.steName . fst) $
            pltStubSymbols pltStubInfo loadOptions ehi
        Nothing ->
          Map.empty

  let pltStubAddrToNameList :: Seq.Seq (MM.MemWord w, WFN.FunctionName)
      pltStubAddrToNameList = Seq.fromList $ Map.toList pltStubAddrToNameMap

  let offset :: Word64
      offset = fromMaybe 0 (LC.loadOffset loadOptions)

  -- This contains PLT stubs that the user specified on the command line.
  let userPltStubAddrToNameList :: Seq.Seq (MM.MemWord w, WFN.FunctionName)
      userPltStubAddrToNameList =
        Seq.fromList $
          map
            ( \(PltStub addr name) ->
                (MM.memWord (addr + offset), WFN.functionNameFromText name)
            )
            userPltStubs

  -- This contains SymbolReloc relocations.
  -- See Note [Subtleties of resolving PLT stubs] (Wrinkle 1: .plt.got) for why
  -- we do this.
  let symbolRelocAddrToNameList :: Seq.Seq (MM.MemWord w, WFN.FunctionName)
      symbolRelocAddrToNameList = Seq.fromList $ Map.toList symbolRelocs

  pltStubSegOffToNameList <-
    traverse
      ( \(addr, name) ->
          case Loader.resolveAbsoluteAddress memory addr of
            Just so -> pure (Right (so, name))
            Nothing ->
              pure (Left (CouldNotResolvePltStub path (MM.memWordValue addr) name))
      )
      ( pltStubAddrToNameList
          <> userPltStubAddrToNameList
          <> symbolRelocAddrToNameList
      )

  case sequence pltStubSegOffToNameList of
    Left err -> pure $ Left err
    Right segOffToNameList -> pure $ Right $ Map.fromList $ Foldable.toList segOffToNameList

{-
Note [Subtleties of resolving PLT stubs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grease must identify external function calls to shared libraries so that it can
skip them. This turns out to be easier said that done, however. This Note aims
to document all of the weird corner cases I have encountered while trying to
make this work.

-----
-- Wrinkle 1: .plt.got
-----

At a first approximation, identifying the external function calls is tantamount
to finding calls to PLT stubs, which are small functions that set up the
necessary scaffolding to call into the global offset table (GOT). Therefore,
if grease invokes an address that corresponds to a PLT stub, then we are about
to invoke an external function. Seems straightforward enough, right?

Sadly, things are more complicated than they appear. The first wrinkle in this
plan is that there are multiple types of PLT stubs. Some PLT stubs appear in the
.plt section, some appear in the .plt.got section, and in rarer cases, some
appear in other sections (e.g., .plt.sec). For the sake of this Note, the one
difference between these PLT stub types that actually matters is that Macaw's
heuristics for identifying PLT stubs (via the `pltStubSymbols` function) are
quite accurate for .plt stubs, but inaccurate for other types of stubs, such
as those that appear in the .plt.got section (see
https://github.com/GaloisInc/macaw/issues/375).

What's more, it is not guaranteed that every external function will have a
corresponding PLT stub. For example, if you compile C code with the -fno-plt
flag, then it will not generate PLT stub functions at all, instead inlining the
code that would have otherwise gone into the bodies of the PLT stubs into the
call sites. In order to identify these external functions, we must do something
other than relying on PLT stubs.

We work around both issues using the same solution. Both .plt.got stubs and
inlined code arising from -fno-plt share the characteristic that the
corresponding entries in the GOT contain GLOB_DAT relocations, which identify
the symbol of the external function.

Normally, the memory that a GLOB_DAT relocation's address points to would not
be known statically, as this is information that would be determined at dynamic
load time. For simulation purposes, however, we can employ a simple hack: we
pretend that this memory contains the address of the relocation itself. The
relocation address is a reasonably unique identifier that we can use to
determine if we are about to call into the function corresponding the the
GLOB_DAT relocation. (See Grease.Macaw.globalMemoryHooks for where this hack is
implemented.)

Therefore, if we are about to call into an address that corresponds to a
GLOB_DAT relocation, then we assume that the GLOB_DAT's symbol is an external
function. This is an over-approximation, as GLOB_DAT relocations can refer to
things besides functions (e.g., global variables), but this over-approximation
is unlikely to cause any harm.

We apply the same workaround to other forms of SymbolRelocs (e.g., R_X86_64_64
on x86-64).

In the event that this workaround isn't enough, we also provide a --plt-stub
command-line flag that allows users to specify the names and addresses of
hard-to-resolve PLT stubs.

-----
-- Wrinkle 2: .plt.sec
-----

Compilers such as gcc and clang support an `-fcf-protection=branch` option that
causes PLT stubs to appear in a .plt.sec section instead of a .plt section.
Moreover, some Linux distributions (e.g., recent versions of Ubuntu) enable
this option by default in gcc, so it is important to be able to handle binaries
that use .plt.sec sections.

I am unclear if there is a precise specification for how
`-fcf-protection=branch` is meant to work, but I have made the following
experimental observations:

1. When a binary is compiled with `-fcf-protection=branch`, the .plt section
   no longer contains PLT stubs, but rather some security-related assembly code
   that I don't quite understand. More importantly, external function calls no
   longer call into the .plt section, so it's not particularly relevant at that
   point.

2. The layout of the .plt.sec section is remarkably similar to the layout of a
   .plt section in a non-`-fcf-protection=branch` binary, and the size of the
   PLT stubs are identical. The one main difference is that there is no function
   at the beginning of the .plt.sec section that precedes all of the PLT stub
   functions, which means that there is no need to use of `pltFunSize` from
   Data.Macaw.Memory.ElfLoader.PLTStubs.

Due to observation (2), calling
Data.Macaw.Memory.ElfLoader.PLTStubs.pltStubSymbols will return the addresses
of all the stubs in the .plt.sec section, but with their addresses offset by a
constant. This constant is equal to the difference between the base addresses
of the .plt.sec and .plt sections, minus the size of `pltFunSize`. As such, a
cheeky way to compute the addresses of the .plt.sec stubs is to take the
results of calling macaw's `pltStubSymbols` function and adding the constant
offset. (See grease's own `pltStubSymbols` function for how this is done.)

Of course, this all relies on the observations above, and if these observations
aren't true in all binaries, then we will need to rethink this. Still, this
does appear to work on a sample of `-fcf-protection=branch` binaries that I
have tried. We may want to consider upstreaming these heuristics to macaw in
the future.

-----
-- Wrinkle 3: PowerPC
-----

PLT stubs on PowerPC work very differently from other architectures.
Unfortunately, this means that Macaw's heuristics aren't able to find any PLT
stubs at all in PowerPC binaries. See
https://github.com/GaloisInc/macaw/issues/386.

To avoid Macaw's heuristics from reporting false positives, we do not attempt
to look for PLT stubs at all when simulating a PowerPC binary. Instead, the
user must supply PLT stubs themselves using the --plt-stub option.
-}

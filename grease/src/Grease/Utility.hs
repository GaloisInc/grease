{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Utility (
  OnlineSolverAndBackend,
  pshow,
  tshow,
  functionNameFromByteString,
  llvmOverrideName,
  ppProgramLoc,
  printHandle,
  segoffToAbsoluteAddr,
  bytes32LE,
  bytes64LE,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32, Word64, Word8)
import Grease.Panic (panic)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as Mem
import Prettyprinter qualified as PP
import System.IO (Handle, stderr)
import Text.LLVM.AST qualified as L
import What4.Expr qualified as W4
import What4.FunctionName qualified as W4
import What4.ProgramLoc qualified as W4
import What4.Protocol.Online qualified as W4

-- | Constraint synonym for using online solver features, e.g. 'W4.checkSatisfiable'
type OnlineSolverAndBackend solver sym bak t st fs =
  ( W4.OnlineSolver solver
  , CB.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder t st fs
  , bak ~ C.OnlineBackend solver t st fs
  )

pshow :: PP.Pretty a => a -> Text
pshow = tshow . PP.pretty

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Decode a 'BS.ByteString' containing UTF-8–encoded text into a
-- 'W4.FunctionName'.
functionNameFromByteString :: BS.ByteString -> W4.FunctionName
functionNameFromByteString = W4.functionNameFromText . Text.decodeUtf8

llvmOverrideName :: Mem.LLVMOverride p sym ext args ret -> W4.FunctionName
llvmOverrideName ov =
  let L.Symbol nm = L.decName (Mem.llvmOverride_declare ov)
   in W4.functionNameFromText (Text.pack nm)

-- TODO(lb): Also print the function name?
ppProgramLoc :: W4.ProgramLoc -> Text
ppProgramLoc = tshow . W4.plSourceLoc

-- | The 'Handle' to write Crucible-related messages to. This includes the
-- output of overrides like @printf@.
--
-- We pick 'stderr' as the 'Handle' so that we can redirect the output of
-- @grease@ to a file without @printf@-related messages cluttering up the
-- output. Among other things, this ensures that we can successfully redirect
-- the output of @grease --json@ and have the resulting standard output be valid
-- JSON even when simulating a program that calls @printf@.
printHandle :: Handle
printHandle = stderr

-- | Convert a 'MC.MemSegmentOff' value to an absolute address.
--
-- Precondition: the 'MC.MemSegmentOff' must be a valid address within the
-- supplied 'MC.Memory'. If this is not the case, this function will panic.
segoffToAbsoluteAddr ::
  MC.MemWidth w => MC.Memory w -> MC.MemSegmentOff w -> MC.MemWord w
segoffToAbsoluteAddr mem segoff =
  case MC.resolveRegionOff mem (MC.addrBase addr) (MC.addrOffset addr) of
    Just addrOff -> MC.segmentOffset seg + MC.segoffOffset addrOff
    Nothing ->
      panic
        "segoffToAbsoluteAddr"
        [ "Failed to resolve absolute address"
        , "Address: " List.++ show addr
        ]
 where
  seg = MC.segoffSegment segoff
  addr = MC.segoffAddr segoff

-- | Split a 'Word32' into a little-endian sequence of bytes.
bytes32LE :: Word32 -> [Word8]
bytes32LE = BSL.unpack . Builder.toLazyByteString . Builder.word32LE

-- | Split a 'Word64' into a little-endian sequence of bytes.
bytes64LE :: Word64 -> [Word8]
bytes64LE = BSL.unpack . Builder.toLazyByteString . Builder.word64LE

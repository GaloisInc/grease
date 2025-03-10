{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Utility
  ( OnlineSolverAndBackend
  , GreaseException(..)
  , pshow
  , tshow
  , functionNameFromByteString
  , declaredFunNotFound
  , llvmOverrideName
  , ppProgramLoc
  , printHandle
  , segoffToAbsoluteAddr
  , bytes32LE
  , bytes64LE
  ) where

import Control.Exception.Safe (MonadThrow, Exception)
import qualified Control.Exception.Safe as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((.))
import qualified Data.List as List
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Type.Equality (type (~))
import Data.Word (Word8, Word32, Word64)
import Prelude ((+))
import System.IO (Handle, stderr)
import Text.Show (Show(..))

import qualified Prettyprinter as PP

import qualified Text.LLVM.AST as L

import qualified What4.FunctionName as W4
import qualified What4.Expr as W4
import qualified What4.ProgramLoc as W4
import qualified What4.Protocol.Online as W4

import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Backend.Online as C

import qualified Lang.Crucible.LLVM.Intrinsics as Mem

import qualified Data.Macaw.CFG as MC

import Grease.Panic (panic)

-- | Constraint synonym for using online solver features, e.g. 'W4.checkSatisfiable'
type OnlineSolverAndBackend solver sym bak t st fs =
  ( W4.OnlineSolver solver
  , C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder t st fs
  , bak ~ C.OnlineBackend solver t st fs
  )

newtype GreaseException = GreaseException Text
instance Exception GreaseException
instance Show GreaseException where
  show (GreaseException msg) = Text.unpack msg

pshow :: PP.Pretty a => a -> Text
pshow = tshow . PP.pretty

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Decode a 'BS.ByteString' containing UTF-8–encoded text into a
-- 'W4.FunctionName'.
functionNameFromByteString :: BS.ByteString -> W4.FunctionName
functionNameFromByteString = W4.functionNameFromText . Text.decodeUtf8

-- | GREASE invoked a forward declaration, but it was unable to resolve the
-- 'C.FnHandle' corresponding to the declaration. Throw an exception suggesting
-- that the user try an override.
declaredFunNotFound :: MonadThrow m => W4.FunctionName -> m a
declaredFunNotFound decName =
  X.throw (GreaseException ("Function declared but not defined: " <> W4.functionName decName <> ". Try specifying an override using --overrides."))

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
    Nothing -> panic
                 "segoffToAbsoluteAddr"
                 [ "Failed to resolve absolute address"
                 , "Address: " List.++ show addr
                 ]
  where
    seg  = MC.segoffSegment segoff
    addr = MC.segoffAddr segoff

-- | Split a 'Word32' into a little-endian sequence of bytes.
bytes32LE :: Word32 -> [Word8]
bytes32LE = BSL.unpack . Builder.toLazyByteString . Builder.word32LE

-- | Split a 'Word64' into a little-endian sequence of bytes.
bytes64LE :: Word64 -> [Word8]
bytes64LE = BSL.unpack . Builder.toLazyByteString . Builder.word64LE

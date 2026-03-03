{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @screach@-specific function overrides for the @sscanf@ family of functions.
module Screach.FunctionOverride.Sscanf (
  -- * Overrides
  sscanfFamilyOverrides,
) where

import Control.Applicative (Alternative (many))
import Control.Lens (to, (^.))
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State qualified as State
import Control.Monad.Trans qualified as Trans
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.BitVector.Sized qualified as BV
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (chr, isSpace, ord)
import Data.Kind (Type)
import Data.Macaw.Architecture.Info qualified as MAI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as MS
import Data.Macaw.Symbolic.MemOps qualified as MSMO
import Data.Maybe (fromMaybe)
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Some (Some (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Stack (HasCallStack, callStack)
import Grease.Macaw.Arch qualified as GMA
import Grease.Macaw.Memory qualified as GMM
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as CT
import Numeric.Natural (Natural)
import Screach.Panic (panic)
import Stubs.FunctionOverride qualified as StubsF
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Byte qualified as TMB
import Text.Megaparsec.Byte.Lexer qualified as TMBL
import What4.FunctionName qualified as WFN
import What4.Interface qualified as WI
import What4.InterpretedFloatingPoint qualified as WFP

-- | All of the supported overrides in the @sscanf@ family of functions,
-- packaged up for your convenience.
sscanfFamilyOverrides ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  ) =>
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  GMA.ArchContext arch ->
  Seq.Seq (StubsF.SomeFunctionOverride p sym arch)
sscanfFamilyOverrides memVar mmConf archCtx =
  Seq.fromList
    [ StubsF.SomeFunctionOverride $ buildSscanfOverride memVar mmConf endian
    , StubsF.SomeFunctionOverride $ buildIsoC99SscanfOverride memVar mmConf endian
    ]
 where
  endian :: MC.Endianness
  endian = archCtx ^. GMA.archInfo . to MAI.archEndianness

-- | Build an override for the @sscanf@ function.
buildSscanfOverride ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  ) =>
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  MC.Endianness ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildSscanfOverride = buildSscanfLikeOverride "sscanf"

-- | Build an override for the @__isoc99_sscanf@ function. This is a common
-- alias for @sscanf@ in various @libc@ implementations, and it is quite likely
-- that when you compile an application of the @sscanf@ function, it will
-- compile down to @__isoc99_sscanf@ at the assembly level, so it is convenient
-- to be able to override this alias as well.
buildIsoC99SscanfOverride ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  ) =>
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  MC.Endianness ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildIsoC99SscanfOverride = buildSscanfLikeOverride "__isoc99_sscanf"

buildSscanfLikeOverride ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  ) =>
  WFN.FunctionName ->
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  MC.Endianness ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildSscanfLikeOverride name memVar mmConf endian =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkVariadicFunctionOverride name $ \bak args getVarArg ->
      Ctx.uncurryAssignment
        ( \inPtr strPtr ->
            callSscanf bak memVar mmConf endian inPtr strPtr getVarArg
        )
        args

-- | Override for the @sscanf@ function (or a similar function with the same type
-- signature). This override is quite incomplete, and although it works well
-- enough for certain use cases, it has a variety of limitations:
--
-- * Both the input string and the format string must be concrete.
--
-- * Only a subset of conversion types and length modifiers are supported at
--   present. Basic conversion types like @%d@, @%c@, @%s@, etc. are supported,
--   but more exotic conversion types may not work as expected.
--
-- * The @%[...]@ conversion type is partially supported. It is capable of
--   expressing a set of characters to include (e.g., @%[abc]@) or to exclude
--   (e.g., @[^abc]@). It does not support expressing ranges or characters,
--   however (e.g., @[0-9a-z]@). It also does not support sets that include the
--   @]@ character (e.g., @[]]@).
--
-- * At present, the maximum width is only respected for string-like conversions
--   (e.g., @%5s@ or @%5[^,]@).
callSscanf ::
  ( CB.IsSymBackend sym bak
  , OnlineSolverAndBackend solver sym bak scope st fm
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  ) =>
  bak ->
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  MC.Endianness ->
  -- | The input pointer
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  -- | The format string
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  -- | The variadic arguments
  StubsF.GetVarArg sym ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callSscanf bak memVar mmConf endian inPtr strPtr gva = do
  -- Read format string
  formatStr <-
    modifyM $
      liftIO . GMM.loadConcreteString bak memVar mmConf strPtr Nothing
  -- Parse format directives
  case parseDirectives (BS.unpack formatStr) of
    Left err ->
      CS.overrideError $
        CS.AssertFailureSimError "Format string parsing failed" err
    Right ds -> do
      -- Compute output
      valist <- liftIO $ getScanfVarArgs (Vec.fromList ds) gva
      inStr <-
        modifyM $
          liftIO . GMM.loadConcreteString bak memVar mmConf inPtr Nothing
      res <-
        evalScanfOpsT inStr $
          executeDirectivesScanf (scanfOps bak memVar mmConf endian valist) ds
      n <-
        case res of
          Left err ->
            CS.overrideError $
              CS.AssertFailureSimError "sscanf input string parse error" $
                TM.errorBundlePretty err
          Right n -> pure n

      nBv <- liftIO $ WI.bvLit sym ?ptrWidth $ BV.mkBV ?ptrWidth $ toInteger n
      liftIO $ CLM.llvmPointer_bv sym nBv
 where
  sym = CB.backendGetSym bak

-- | Given the directives in a @scanf@-style format string, retrieve the
-- corresponding variadic arguments.
getScanfVarArgs ::
  CLM.HasPtrWidth w =>
  Vec.Vector PrintfDirective ->
  StubsF.GetVarArg sym ->
  IO (Vec.Vector (CLM.LLVMPtr sym w))
getScanfVarArgs pds =
  State.evalStateT (Vec.mapMaybeM (State.StateT . getScanfVarArg) pds)

-- | Given a single directive in a @scanf@-style format string:
--
-- * If it is a conversion directive (i.e., beginning with a @%@ character),
--   retrieve a variadic argument @arg@ return @('Just' arg, gva)@, where @gva@
--   is the callback for retrieving the next variadic argument.
--
-- * Otherwise, return @('Nothing', gva)@.
getScanfVarArg ::
  CLM.HasPtrWidth w =>
  PrintfDirective ->
  StubsF.GetVarArg sym ->
  IO (Maybe (CLM.LLVMPtr sym w), StubsF.GetVarArg sym)
getScanfVarArg pd gva@(StubsF.GetVarArg getVarArg) =
  case pd of
    StringDirective{} -> pure (Nothing, gva)
    ConversionDirective{} -> do
      (CS.RegEntry _ val, gva') <- getVarArg CLM.PtrRepr
      pure (Just val, gva')

-- | Like 'PrintfOperations', but geared specifically towards the needs of
-- the @scanf@ family of functions.
data ScanfOperations m
  = ScanfOperations
  { scanfSetInteger ::
      Int -> -- Field number
      Bool -> -- is Signed?
      PrintfLengthModifier ->
      Int -> -- Number of bytes
      Integer ->
      m ()
  , scanfSetFloat ::
      Int -> -- FieldNumber
      PrintfLengthModifier ->
      Int -> -- Number of bytes
      Rational ->
      m ()
  , scanfSetPointer ::
      Int -> -- FieldNumber
      Integer ->
      m ()
  , scanfSetString ::
      Int -> -- FieldNumber
      Maybe Int -> -- Number of chars to read; if Nothing, read until null terminator
      [Word8] ->
      m ()
  , scanfUnsupported :: !(forall a. HasCallStack => String -> m a)
  }

-- | Given a list of directives in a @scanf@ format string and a set of
-- 'ScanfOperations' to perform, parse the input string and perform the
-- appropriate operation for each directive. Note that the input string is not
-- an explicit input to this function; that is implicit in the 'String' part of
-- the @'TM.MonadParsec' 'Void' 'BS.ByteString'@ constraint.
--
-- If we wanted to more faithfully implement the semantics of the @scanf@
-- family of functions, we would need to pay more careful attention to things
-- like modifiers while parsing.
executeDirectivesScanf ::
  forall m.
  TM.MonadParsec Void BS.ByteString m =>
  ScanfOperations m ->
  [PrintfDirective] ->
  m Int
executeDirectivesScanf ops directives = go 0 directives
 where
  go :: Int -> [PrintfDirective] -> m Int
  go !_fld [] = do
    TM.eof
    pure (countConversionDirectives directives)
  go fld (StringDirective bs : xs) = do
    _ <- TMB.string bs
    go fld xs
  go fld (ConversionDirective d : xs) =
    let fld' = fromMaybe (fld + 1) (printfAccessField d)
     in case printfType d of
          ConversionInteger fmt -> do
            let sgn = signedIntFormat fmt
            let intParser :: m Integer
                intParser = TMBL.signed (pure ()) TMBL.decimal
            let numBytes :: Int
                numBytes =
                  case printfLengthMod d of
                    LenByte -> 1
                    LenLong -> 8
                    LenLongLong -> 8
                    _ -> 4
            i <- intParser
            scanfSetInteger ops fld' sgn (printfLengthMod d) numBytes i
            go fld' xs
          ConversionFloating _fmt -> do
            let floatParser :: m Double
                floatParser = TMBL.signed (pure ()) TMBL.float
            let numBytes :: Int
                numBytes =
                  case printfLengthMod d of
                    LenLongDouble -> 8
                    _ -> 4
            f <- floatParser
            scanfSetFloat ops fld' (printfLengthMod d) numBytes (toRational f)
            go fld' xs
          ConversionString -> do
            let strParser :: m BS.ByteString
                strParser = TM.takeWhileP (Just "%s") (not . isSpace . chr8)
            s <- takeMaximumWidth d <$> strParser
            scanfSetString ops fld' (printfPrecision d) (BS.unpack s)
            go fld' xs
          ConversionChar -> do
            let charParser :: m Word8
                charParser = TM.anySingle
            w8 <- charParser
            let sgn = False -- unsigned
            scanfSetInteger ops fld' sgn LenNoMod 1 $ toInteger w8
            go fld' xs
          ConversionPointer -> do
            let ptrParser :: m Integer
                ptrParser = TMB.char (ord8 '0') *> TMB.char (ord8 'x') *> TMBL.hexadecimal
            ptr <- ptrParser
            scanfSetPointer ops fld' ptr
            go fld' xs
          ConversionCountChars -> do
            let sgn = False -- unsigned
            len <- TM.getOffset
            scanfSetInteger ops fld' sgn (printfLengthMod d) 4 (toInteger len)
            go fld' xs
          ConversionCharSet chars inclusive -> do
            let shouldTake :: Char -> Bool
                shouldTake c
                  | inclusive = c `Set.member` chars
                  | otherwise = not (c `Set.member` chars)

                strParser :: m BS.ByteString
                strParser = TM.takeWhileP (Just "%[...]") (shouldTake . chr8)
            s <- takeMaximumWidth d <$> strParser
            scanfSetString ops fld' (printfPrecision d) (BS.unpack s)
            go fld' xs

  -- If a conversion directive specifies a maximum width, then only take as
  -- many bytes as permitted. Otherwise, return the original bytestring
  -- unchanged.
  takeMaximumWidth :: ConversionDirective -> BS.ByteString -> BS.ByteString
  takeMaximumWidth d =
    case printfMaxWidth d of
      Just width -> BS.take width
      Nothing -> id

-- | Like 'C.chr', but for 'Word8' instead of 'Int'.
chr8 :: Word8 -> Char
chr8 = chr . fromIntegral

-- | Like 'C.ord', but for 'Word8' instead of 'Int'.
ord8 :: Char -> Word8
ord8 = fromIntegral . ord

-- | Count the number of 'ConversionDirectives' in a format string. This
-- is precisely the value that the @scanf@ family of functions is meant to
-- return.
countConversionDirectives :: [PrintfDirective] -> Int
countConversionDirectives = length . filter isConversionDirective
 where
  isConversionDirective :: PrintfDirective -> Bool
  isConversionDirective ConversionDirective{} = True
  isConversionDirective StringDirective{} = False

-- | An auxiliary monad transformer used in 'scanfOps'. This adds a 'TM.ParsecT'
-- effect for parsing a @scanf@ format string.
type ScanfOpsT :: (Type -> Type) -> Type -> Type
newtype ScanfOpsT m a = ScanfOpsT (TM.ParsecT Void BS.ByteString m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , MonadIO
    , TM.MonadParsec Void BS.ByteString
    , State.MonadState s
    , Trans.MonadTrans
    )

evalScanfOpsT ::
  Monad m =>
  BS.ByteString ->
  ScanfOpsT m a ->
  m (Either (TM.ParseErrorBundle BS.ByteString Void) a)
evalScanfOpsT str (ScanfOpsT m) = TM.runParserT m "" str

-- | Define handlers for the various @scanf@ directives.
scanfOps ::
  forall m sym bak w arch p ext r args ret solver scope st fm.
  ( CB.IsSymBackend sym bak
  , OnlineSolverAndBackend solver sym bak scope st fm
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  , m ~ ScanfOpsT (CS.OverrideSim p sym ext r args ret)
  ) =>
  bak ->
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  MC.Endianness ->
  -- | Variadic arguments
  Vec.Vector (CLM.LLVMPtr sym w) ->
  ScanfOperations m
scanfOps bak memVar mmConf endian valist =
  ScanfOperations
    { scanfUnsupported = \x ->
        Trans.lift $
          liftIO $
            CB.addFailedAssertion bak $
              CS.Unsupported callStack x
    , scanfSetInteger = \i _sgn _len numBytes int ->
        withArgIndex i $ \ptr -> storeInteger ptr numBytes int
    , scanfSetFloat = \i _len numBytes rat ->
        withArgIndex i $ \ptr -> storeRational ptr numBytes rat
    , scanfSetPointer = \i ptrVal ->
        withArgIndex i $ \ptr -> storeInteger ptr 8 ptrVal
    , scanfSetString = \i _numchars str ->
        withArgIndex i $ \ptr ->
          let ptrEntry = CS.RegEntry CLM.PtrRepr ptr
           in stateM $
                liftIO . storeConcreteString bak memVar mmConf endian ptrEntry (BS.pack str)
    }
 where
  sym = CB.backendGetSym bak

  withArgIndex ::
    Int ->
    (CLM.LLVMPtr sym w -> m ()) ->
    m ()
  withArgIndex i k =
    case valist Vec.!? (i - 1) of
      Just ptr -> k ptr
      Nothing ->
        liftIO $
          CB.addFailedAssertion bak $
            CS.AssertFailureSimError
              "Out-of-bounds argument access in scanf"
              (unwords ["Index:", show i])

  store ::
    forall ty.
    MC.MemRepr ty ->
    CS.RegEntry sym (CLM.LLVMPointerType w) ->
    CS.RegEntry sym (MS.ToCrucibleType ty) ->
    m ()
  store writeInfo pDst pSrc =
    let addrWidth = MC.addrWidthRepr ?ptrWidth
     in stateM $
          liftIO . MS.doWriteMemModel memVar mmConf addrWidth writeInfo pDst pSrc

  storeInteger :: CLM.LLVMPtr sym w -> Int -> Integer -> m ()
  storeInteger pDst numBytes int = do
    Some (bytesRepr :: NatRepr.NatRepr bytes) <-
      pure $ NatRepr.mkNatRepr $ fromIntegral @Int @Natural numBytes
    case NatRepr.isPosNat bytesRepr of
      Nothing ->
        panic "scanfOps" ["Zero-width integer size"]
      Just NatRepr.LeqProof -> do
        let bitsRepr = NatRepr.natMultiply (NatRepr.knownNat @8) bytesRepr
        NatRepr.LeqProof <-
          pure $
            NatRepr.leqMulCongr
              (NatRepr.LeqProof @1 @8)
              (NatRepr.LeqProof @1 @bytes)
        let writeInfo = MC.BVMemRepr bytesRepr endian
        let pDstEntry = CS.RegEntry CLM.PtrRepr pDst
        symBv <- liftIO $ WI.bvLit sym bitsRepr $ BV.mkBV bitsRepr int
        pSrc <- liftIO $ CLM.llvmPointer_bv sym symBv
        let pSrcEntry = CS.RegEntry (CLM.LLVMPointerRepr bitsRepr) pSrc
        store writeInfo pDstEntry pSrcEntry

  storeRational :: CLM.LLVMPtr sym w -> Int -> Rational -> m ()
  storeRational pDst numBytes rat = do
    let unsupportedFloatSize :: forall a. a
        unsupportedFloatSize =
          panic "scanfOps" ["Unsupported float size", show numBytes]
    Some floatInfoRepr <- pure $
      case numBytes of
        4 -> Some MC.SingleFloatRepr
        8 -> Some MC.DoubleFloatRepr
        _ -> unsupportedFloatSize
    let writeInfo = MC.FloatMemRepr floatInfoRepr endian
    let pDstEntry = CS.RegEntry CLM.PtrRepr pDst
    symFloat <- liftIO $
      case floatInfoRepr of
        MC.SingleFloatRepr -> WFP.iFloatLitSingle sym $ fromRational rat
        MC.DoubleFloatRepr -> WFP.iFloatLitDouble sym $ fromRational rat
        _ -> unsupportedFloatSize
    let floatEntry =
          CS.RegEntry
            (CT.FloatRepr (MS.floatInfoToCrucible floatInfoRepr))
            symFloat
    store writeInfo pDstEntry floatEntry

-- | Given a function that modifies state and returns a new value, this function
-- wraps the call in @get@ and @put@ operations to update the state in the state
-- monad.
modifyM :: State.MonadState s m => (s -> m (a, s)) -> m a
modifyM fn = do
  s <- State.get
  (a, s') <- fn s
  State.put s'
  return a

-- | Given a function that modifies state, this function wraps the call in
-- @get@ and @put@ operations to update the state in the state monad.
stateM :: State.MonadState s m => (s -> m s) -> m ()
stateM fn = do
  s <- State.get
  s' <- fn s
  State.put s'

-- | Store a string (represented as a list of bytes) to memory,
-- including a null terminator at the end. We permit symbolic bytes, but the
-- null terminator written at the end will always be concrete.
storeString ::
  forall sym bak p arch rtp f args solver t st fm.
  ( OnlineSolverAndBackend solver sym bak t st fm
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  -- | The global variable to the LLVM memory.
  CS.GlobalVar CLM.Mem ->
  -- | The @macaw-symbolic@ memory model configuration.
  MS.MemModelConfig p sym arch CLM.Mem ->
  -- | The endianness of the architecture.
  MC.Endianness ->
  -- | The pointer to write the string to.
  CS.RegEntry sym (CLM.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  -- | The bytes of the string to write (null terminator not included).
  [WI.SymBV sym 8] ->
  -- | The initial Crucible state.
  CS.SimState p sym (MS.MacawExt arch) rtp f args ->
  -- | The updated Crucible state.
  IO (CS.SimState p sym (MS.MacawExt arch) rtp f args)
storeString bak mvar mmConf endian ptr bytes st = do
  let sym = CB.backendGetSym bak
  let byteRepr = WI.knownNat @8

  let go ::
        CS.RegEntry sym (CLM.LLVMPointerType (MC.ArchAddrWidth arch)) ->
        [WI.SymBV sym 8] ->
        CS.SimState p sym (MS.MacawExt arch) rtp f args ->
        IO (CS.SimState p sym (MS.MacawExt arch) rtp f args)
      go p bvBytes st0 =
        case bvBytes of
          [] -> do
            bvNullTerminator <- liftIO $ WI.bvLit sym byteRepr $ BV.zero byteRepr
            writeByte p bvNullTerminator st0
          bvByte : bs -> do
            st1 <- writeByte p bvByte st0
            bvOne <- liftIO $ WI.bvLit sym CLM.PtrWidth (BV.one CLM.PtrWidth)
            memImpl <- MSMO.getMem st1 mvar
            p' <- liftIO $ CLM.doPtrAddOffset bak memImpl (CS.regValue p) bvOne
            go (CS.RegEntry CLM.PtrRepr p') bs st1

      writeByte ::
        CS.RegEntry sym (CLM.LLVMPointerType (MC.ArchAddrWidth arch)) ->
        WI.SymBV sym 8 ->
        CS.SimState p sym (MS.MacawExt arch) rtp f args ->
        IO (CS.SimState p sym (MS.MacawExt arch) rtp f args)
      writeByte p bvByte st0 = do
        let addrWidth = MC.addrWidthRepr ?ptrWidth
        let writeInfo = MC.BVMemRepr (WI.knownNat @1) endian
        ptrByte <- liftIO $ CLM.llvmPointer_bv sym bvByte
        let ptrByte' = CS.RegEntry (CLM.LLVMPointerRepr byteRepr) ptrByte
        MS.doWriteMemModel mvar mmConf addrWidth writeInfo p ptrByte' st0

  go ptr bytes st

-- | Like 'storeString', except that each character in the string is concrete.
storeConcreteString ::
  forall sym bak p arch rtp f args solver t st fm.
  ( OnlineSolverAndBackend solver sym bak t st fm
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  -- | The global variable to the LLVM memory.
  CS.GlobalVar CLM.Mem ->
  -- | The @macaw-symbolic@ memory model configuration.
  MS.MemModelConfig p sym arch CLM.Mem ->
  -- | The endianness of the architecture.
  MC.Endianness ->
  -- | The pointer to write the string to.
  CS.RegEntry sym (CLM.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  -- | The bytes of the string to write (null terminator not included).
  BS.ByteString ->
  -- | The initial Crucible state.
  CS.SimState p sym (MS.MacawExt arch) rtp f args ->
  -- | The updated Crucible state.
  IO (CS.SimState p sym (MS.MacawExt arch) rtp f args)
storeConcreteString bak mvar mmConf endian ptr bytes st = do
  symBytes <-
    liftIO $
      traverse
        (WI.bvLit sym byteRepr . BV.mkBV byteRepr . toInteger)
        (BS.unpack bytes)
  storeString bak mvar mmConf endian ptr symBytes st
 where
  sym = CB.backendGetSym bak
  byteRepr = WI.knownNat @8

-----
-- The code below is mostly copy-pasted from crucible-llvm's
-- Lang.Crucible.LLVM.Printf module (BSD-3-licensed). This is because @printf@
-- and @sscanf@ share a lot of internal machinery, so although many of the names
-- below mention @printf@, the same concepts often apply directly to @sscanf@
-- as well. Nevertheless, there are some key differences between the two
-- functions, and the differences are subtle enough to the point where I wasn't
-- sure how to reuse the existing machinery as-is, so I decided to copy it
-- wholesale and make whatever changes I saw fit.
--
-- I have listed the changes I made below:
--

-- * We have added support for 'Conversion_CharSet' (i.e., the @[...]@

--   conversion type).
--

-- * The meaning of @%<number>@ in a conversion type differs between @printf@

--   and @sscanf@. In @printf@, the @<number>@ is a _minimum_ width for the
--   field, but in @sscanf@, the @<number>@ is a _maximum_ width for the field.
--   As such, we have renamed 'printfMinWidth' to 'printfMaxWidth'.
-----

data PrintfFlag
  = PrintfAlternateForm -- #
  | PrintfZeroPadding -- 0
  | PrintfNegativeWidth -- -
  | PrintfPosSpace -- ' '
  | PrintfPosPlus -- +
  | PrintfThousandsSep -- '
  deriving (Eq, Ord, Show)

data PrintfLengthModifier
  = LenByte -- hh
  | LenShort -- h
  | LenLong -- l
  | LenLongLong -- ll
  | LenLongDouble -- L
  | LenIntMax -- j
  | LenPtrDiff -- t
  | LenSizet -- z
  | LenNoMod -- <<no length modifier>>
  deriving (Eq, Ord, Show)

data Case
  = UpperCase
  | LowerCase
  deriving (Eq, Ord, Show)

data IntFormat
  = IntFormatSignedDecimal -- i,d
  | IntFormatUnsignedDecimal -- u
  | IntFormatOctal -- o
  | IntFormatHex Case -- x,X
  deriving (Eq, Ord, Show)

signedIntFormat :: IntFormat -> Bool
signedIntFormat IntFormatSignedDecimal = True
signedIntFormat _ = False

data FloatFormat
  = FloatFormatScientific Case -- e,E
  | FloatFormatStandard Case -- f,F
  | FloatFormatAuto Case -- g,G
  | FloatFormatHex Case -- a,A
  deriving (Eq, Ord, Show)

data PrintfConversionType
  = ConversionInteger IntFormat
  | ConversionFloating FloatFormat
  | ConversionChar -- c
  | ConversionString -- s
  | ConversionPointer -- p
  | ConversionCountChars -- n
  | -- |
    -- * @[...]@  (if the 'Bool' is 'True', i.e., include characters in the 'Set')
    --
    -- * @[^...]@ (if the 'Bool' is 'False', i.e., exclude characters in the 'Set')
    ConversionCharSet (Set Char) Bool
  deriving (Eq, Ord, Show)

data PrintfDirective
  = StringDirective BS.ByteString
  | ConversionDirective ConversionDirective
  deriving (Eq, Ord, Show)

data ConversionDirective = Conversion
  { printfAccessField :: Maybe Int
  , printfFlags :: Set PrintfFlag
  , printfMaxWidth :: Maybe Int
  , printfPrecision :: Maybe Int
  , printfLengthMod :: PrintfLengthModifier
  , printfType :: PrintfConversionType
  }
  deriving (Eq, Ord, Show)

parseDirectives :: [Word8] -> Either String [PrintfDirective]
parseDirectives xs =
  Atto.parseOnly (parseFormatString <* Atto.endOfInput) (BS.pack xs)

parseFormatString :: Atto.Parser [PrintfDirective]
parseFormatString =
  many $
    Atto.choice
      [ StringDirective <$> Atto.takeWhile1 (/= '%')
      , Atto.string "%%" >> return (StringDirective "%")
      , parseConversion
      ]

parseConversion :: Atto.Parser PrintfDirective
parseConversion = do
  _ <- Atto.char '%'
  field <-
    Atto.option
      Nothing
      ( Just
          <$> do
            d <- Atto.decimal
            _ <- Atto.char '$'
            return d
      )
  flags <- parseFlags Set.empty
  width <- Atto.option Nothing (Just <$> Atto.decimal)
  prec <- Atto.option Nothing (Atto.char '.' >> (Just <$> Atto.decimal))
  len <- parseLenModifier
  typ <- parseConversionType
  return $
    ConversionDirective $
      Conversion
        { printfAccessField = field
        , printfFlags = flags
        , printfMaxWidth = width
        , printfPrecision = prec
        , printfLengthMod = len
        , printfType = typ
        }

parseFlags :: Set PrintfFlag -> Atto.Parser (Set PrintfFlag)
parseFlags fs =
  Atto.choice
    [ Atto.char '#' >> parseFlags (Set.insert PrintfAlternateForm fs)
    , Atto.char '0' >> parseFlags (Set.insert PrintfZeroPadding fs)
    , Atto.char '-' >> parseFlags (Set.insert PrintfNegativeWidth fs)
    , Atto.char ' ' >> parseFlags (Set.insert PrintfPosSpace fs)
    , Atto.char '+' >> parseFlags (Set.insert PrintfPosPlus fs)
    , Atto.char '\'' >> parseFlags (Set.insert PrintfThousandsSep fs)
    , return fs
    ]

parseLenModifier :: Atto.Parser PrintfLengthModifier
parseLenModifier =
  Atto.choice
    [ Atto.string "hh" >> return LenByte
    , Atto.string "h" >> return LenShort
    , Atto.string "ll" >> return LenLongLong
    , Atto.string "L" >> return LenLongDouble
    , Atto.string "l" >> return LenLong
    , Atto.string "j" >> return LenIntMax
    , Atto.string "t" >> return LenPtrDiff
    , Atto.string "z" >> return LenSizet
    , return LenNoMod
    ]

-- | Parse a set of characters to include or exclude. See the Haddocks for
-- 'callSscanf' for the various limitations on how this parser works.
parseCharSet :: Atto.Parser PrintfConversionType
parseCharSet = do
  chars <- BSC.unpack <$> Atto.takeWhile1 (/= ']')
  pure $
    case chars of
      '^' : rest -> ConversionCharSet (Set.fromList rest) False
      _ -> ConversionCharSet (Set.fromList chars) True

parseConversionType :: Atto.Parser PrintfConversionType
parseConversionType =
  Atto.choice
    [ Atto.char 'd' >> return (ConversionInteger IntFormatSignedDecimal)
    , Atto.char 'i' >> return (ConversionInteger IntFormatSignedDecimal)
    , Atto.char 'u' >> return (ConversionInteger IntFormatUnsignedDecimal)
    , Atto.char 'o' >> return (ConversionInteger IntFormatOctal)
    , Atto.char 'x' >> return (ConversionInteger (IntFormatHex LowerCase))
    , Atto.char 'X' >> return (ConversionInteger (IntFormatHex UpperCase))
    , Atto.char 'e' >> return (ConversionFloating (FloatFormatScientific LowerCase))
    , Atto.char 'E' >> return (ConversionFloating (FloatFormatScientific UpperCase))
    , Atto.char 'f' >> return (ConversionFloating (FloatFormatStandard LowerCase))
    , Atto.char 'F' >> return (ConversionFloating (FloatFormatStandard UpperCase))
    , Atto.char 'g' >> return (ConversionFloating (FloatFormatAuto LowerCase))
    , Atto.char 'G' >> return (ConversionFloating (FloatFormatAuto UpperCase))
    , Atto.char 'a' >> return (ConversionFloating (FloatFormatHex LowerCase))
    , Atto.char 'A' >> return (ConversionFloating (FloatFormatHex UpperCase))
    , Atto.char 'c' >> return ConversionChar
    , Atto.char 's' >> return ConversionString
    , Atto.char 'p' >> return ConversionPointer
    , Atto.char 'n' >> return ConversionCountChars
    , Atto.char '[' >> parseCharSet <* Atto.char ']'
    ]

{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- See @doc/dev.md@ for a description of how tests are organized
module Main (main) where

import Control.Exception qualified as X
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.FileEmbed (embedFileRelative)
import Data.Functor ((<&>))
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Traversable (for)
import Grease.Cli (optsFromList)
import Grease.Diagnostic (Diagnostic, GreaseLogAction)
import Grease.Main (simulateFile, logResults)
import Grease.Options (SimOpts(..), optsSimOpts)
import HsLua (Lua)
import HsLua qualified as Lua
import Lumberjack qualified as LJ
import Oughta qualified
import Prelude hiding (fail)
import Prettyprinter qualified as PP
import Shape (shapeTests)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import Test.Tasty qualified as T
import Test.Tasty.HUnit qualified as T.U

prelude :: Text.Text
prelude = Text.decodeUtf8 $(embedFileRelative "tests/test.lua")

data Arch = Armv7 | PPC32 | X64
  deriving Eq

ppArch :: Arch -> String
ppArch Armv7 = "armv7l"
ppArch PPC32 = "ppc32"
ppArch X64 = "x64"

parseArch :: String -> Arch
parseArch s =
  if | s == ppArch Armv7 -> Armv7
     | s == ppArch PPC32 -> PPC32
     | s == ppArch X64 -> X64
     | otherwise -> error ("Unknown architecture: " ++ s)

-- Format for architecture-specific directives in comments in test files
archComment :: Arch -> Text.Text
archComment =
  \case
    Armv7 -> "arm"
    PPC32 -> "ppc32"
    X64 -> "x64"

capture ::
  MonadIO m =>
  IORef.IORef [Diagnostic] ->
  LJ.LogAction m Diagnostic
capture logRef =
  LJ.LogAction (\msg -> liftIO (IORef.modifyIORef logRef (msg :)))

withCapturedLogs ::
  (GreaseLogAction -> IO ()) ->
  IO Text.Text
withCapturedLogs withLogAction = do
  logRef <- IORef.newIORef []
  withLogAction (capture logRef)
  -- Reverse the list so that logs appear chronologically
  logs <- List.reverse <$> IORef.readIORef logRef
  let logTxt = Text.unlines (map (Text.pack . show . PP.pretty) logs)
  pure logTxt

go :: String -> Lua ()
go prog = do
  strOpts <- getArgs
  Lua.newtable
  Lua.setglobal argsGlobal

  opts <- liftIO (optsSimOpts <$> optsFromList (prog : strOpts))
  let action =
        withCapturedLogs $ \la' -> do
          res <- simulateFile opts la'
          logResults la' res
  logTxt <-
    liftIO $
      X.try @X.SomeException action <&>
        \case
          Left e -> Text.pack ("Exception: " ++ show e)
          Right t -> t
  let path = simProgPath opts
  liftIO (Text.IO.writeFile (FilePath.replaceExtension path "out") logTxt)

  Lua.getglobal' "reset"
  Lua.pushstring "<out>"
  Lua.pushstring (Text.encodeUtf8 logTxt)
  Lua.call 2 0

argsGlobal :: Lua.Name
argsGlobal = Lua.Name "_grease_args"

-- Get the arguments stored in 'argsGlobal'
getArgs :: Lua [String]
getArgs = do
  _ty <- Lua.getglobal argsGlobal
  l <- fromIntegral <$> Lua.rawlen Lua.top
  forM [1..l] $ \i -> do
    Lua.pushinteger i
    _ty <- Lua.gettable (Lua.nth 2)
    flag <- Lua.tostring Lua.top
    Lua.pop 1
    let err = error "getArgs: Expected a string"
    let toStr = Text.unpack . Text.decodeUtf8Lenient
    return (toStr (Maybe.fromMaybe err flag))

-- Append a list of 'String's to a Lua array-style (int-keyed) table that is on
-- top of the stack.
appendStringArray :: [String] -> Lua ()
appendStringArray strs = do
  l <- fromIntegral <$> Lua.rawlen Lua.top
  forM_ (zip [1..] strs) $ \(i, s) -> do
    Lua.pushinteger (l + i)
    Lua.pushstring (Text.encodeUtf8 (Text.pack s))
    Lua.settable (Lua.nth 3)

flags :: [String] -> Lua ()
flags fs = do
  _ty <- Lua.getglobal argsGlobal
  appendStringArray fs
  Lua.pop 1

preHook :: FilePath -> Lua ()
preHook bin = do
  Lua.newtable
  Lua.setglobal argsGlobal

  Lua.pushstring (Text.encodeUtf8 (Text.pack bin))
  Lua.setglobal (Lua.Name "prog")

  Lua.pushHaskellFunction (Lua.toHaskellFunction flags)
  Lua.setglobal (Lua.Name "flags")

  Lua.pushHaskellFunction (Lua.toHaskellFunction go)
  Lua.setglobal (Lua.Name "go")

-- | Make an "Oughta"-based test
oughta :: FilePath -> Oughta.LuaProgram -> IO ()
oughta bin prog0 = do
  let output = Oughta.Output ""  -- set by `go`
  let prog = Oughta.addPrefix prelude prog0
  let hooks = Oughta.defaultHooks { Oughta.preHook = preHook bin }
  Oughta.Result r <- Oughta.check hooks prog output
  case r of
    Left f -> X.throwIO f
    Right s ->
      let ms = Oughta.successMatches s in
      T.U.assertBool "Test has some assertions" (not (Seq.null ms))

-- | Make a "Oughta"-based tests for binaries
oughtaBin ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  T.TestTree
oughtaBin dir fileName = do
  let drop1 = FilePath.dropExtension fileName
  let drop2 = FilePath.dropExtension drop1
  let archName = List.drop 1 (FilePath.takeExtension drop1)
  let arch = parseArch archName
  T.U.testCase (ppArch arch) $ do
    let c = dir </> FilePath.addExtension drop2 "c"
    content <- Text.IO.readFile c
    let isArchComment = Text.stripPrefix ("// " <> archComment arch <> ":")
    let isGenericComment = Text.stripPrefix "// all: "
    let isLuaComment = isArchComment <> isGenericComment
    let prog = Oughta.fromLines c isLuaComment content
    oughta (dir </> fileName) prog

-- | Make an "Oughta"-based test for an S-expression program
oughtaSexp ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  T.TestTree
oughtaSexp dir fileName =
  T.U.testCase (FilePath.dropExtension (FilePath.dropExtension fileName)) $ do
    let path = dir </> fileName
    content <- Text.IO.readFile path
    let prog = Oughta.fromLineComments path ";; " content
    oughta (dir </> fileName) prog

-- | Make an "Oughta"-based test for an LLVM bitcode program
oughtaBc ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  T.TestTree
oughtaBc dir fileName =
  let dropped = FilePath.dropExtension fileName in
  T.U.testCase dropped $ do
    let c = dir </> FilePath.addExtension dropped  "c"
    content <- Text.IO.readFile c
    let prog = Oughta.fromLineComments c "/// " content
    oughta (dir </> fileName) prog

-- | Create a test from a file, depending on the extension
fileTest :: FilePath -> FilePath -> [T.TestTree]
fileTest d f =
  case FilePath.takeExtension f of
    ".bc" -> [oughtaBc d f]
    ".cbl" -> [oughtaSexp d f]
    ".elf" -> [oughtaBin d f]
    _ -> []

-- | Recursively walk a directory tree, discovering tests
discoverTests :: FilePath -> IO T.TestTree
discoverTests d = do
  entries <- Dir.listDirectory d
  fmap (T.testGroup (FilePath.takeBaseName d) . List.concat) $
    for entries $ \ent -> do
      -- skip test support files
      if ent == "extra" || ".aux" `List.isInfixOf` ent
      then pure []
      else do
        let path = d </> ent
        isDir <- Dir.doesDirectoryExist path
        if isDir
        then do
          tests <- discoverTests path
          pure [tests]
        else pure (fileTest d ent)

main :: IO ()
main = do
  -- Each entry in this list should be documented in doc/dev.md
  let dirs =
        [ "arm"
        , "llvm"
        , "llvm-bc"
        , "ppc32"
        , "prop"
        , "refine"
        , "sanity"
        , "x86"
        ]
  tests <- mapM discoverTests (map ("tests" </>) dirs)
  T.defaultMain $ T.testGroup "Tests" (shapeTests:tests)

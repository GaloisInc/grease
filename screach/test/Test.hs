-- See @doc/dev.md@ for a high-level description of the test suite
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- `error` is fine in a test suite
{- HLINT ignore "Use panic" -}

module Main (main) where

import Control.Exception qualified as X
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.FileEmbed (embedFileRelative)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Traversable (for)
import HsLua (Lua)
import HsLua qualified as Lua
import Lumberjack qualified as LJ
import Oughta qualified
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Screach qualified
import Screach.Cli qualified as Cli
import Screach.Config qualified as Conf
import Screach.Diagnostic (Diagnostic, ScreachLogAction)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import Test.Tasty qualified as TT
import Test.Tasty.HUnit qualified as TTH

prelude :: Text.Text
prelude = Text.decodeUtf8Lenient $(embedFileRelative "test/test.lua")

capture ::
  MonadIO m =>
  IORef.IORef [Diagnostic] ->
  LJ.LogAction m Diagnostic
capture logRef =
  LJ.LogAction (\msg -> liftIO (IORef.modifyIORef logRef (msg :)))

withCapturedLogs ::
  (ScreachLogAction -> IO ()) ->
  IO Text.Text
withCapturedLogs withLogAction = do
  logRef <- IORef.newIORef []
  withLogAction (capture logRef)
  -- Reverse the list so that logs appear chronologically
  logs <- List.reverse <$> IORef.readIORef logRef
  let ppLog = PP.renderStrict . PP.layoutCompact . PP.pretty
  let logTxt = Text.unlines (map ppLog logs)
  pure logTxt

go :: String -> Lua ()
go prog = do
  strOpts <- getArgs
  Lua.newtable
  Lua.setglobal argsGlobal

  opts <- liftIO (Cli.cliFromList (prog : strOpts))
  let action = withCapturedLogs (Screach.runScreach opts)
  logTxt <-
    liftIO $
      X.try @X.SomeException action
        <&> \case
          Left e -> Text.pack ("Exception: " ++ show e)
          Right t -> t
  let path = Conf.confProgram $ Conf.programConfig opts
  liftIO (Text.IO.writeFile (FilePath.replaceExtension path "out") logTxt)

  Lua.getglobal' "reset"
  Lua.pushstring "<out>"
  Lua.pushstring (Text.encodeUtf8 logTxt)
  Lua.call 2 0

argsGlobal :: Lua.Name
argsGlobal = Lua.Name "_screach_args"

-- Get the arguments stored in 'argsGlobal'
getArgs :: Lua [String]
getArgs = do
  _ty <- Lua.getglobal argsGlobal
  l <- fromIntegral <$> Lua.rawlen Lua.top
  Monad.forM [1 .. l] $ \i -> do
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
  Monad.forM_ (zip [1 ..] strs) $ \(i, s) -> do
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
oughta ::
  FilePath ->
  Oughta.LuaProgram ->
  IO ()
oughta path prog0 = do
  let output = Oughta.Output "" -- set by `go`
  let prog = Oughta.addPrefix prelude prog0
  let hooks = Oughta.defaultHooks{Oughta.preHook = preHook path}
  Oughta.Result r <- Oughta.check hooks prog output
  case r of
    Left f -> X.throwIO f
    Right s ->
      let ms = Oughta.successMatches s
       in TTH.assertBool "Test has some assertions" (not (Foldable.null ms))

-- | Make a "Oughta"-based tests for binaries
oughtaElf ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  TT.TestTree
oughtaElf dir fileName = do
  let base = FilePath.dropExtension fileName
  TTH.testCase base $ do
    let c = dir </> FilePath.addExtension base "c"
    content <- Text.IO.readFile c
    let isLuaComment = Text.stripPrefix "///"
    let prog = Oughta.fromLines c isLuaComment content
    oughta (dir </> fileName) prog

-- | Make an "Oughta"-based test for an S-expression program
oughtaSexp ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  TT.TestTree
oughtaSexp dir fileName =
  let base = FilePath.dropExtension fileName
   in TTH.testCase base $ do
        let path = dir </> fileName
        content <- Text.IO.readFile path
        let prog = Oughta.fromLineComments path ";; " content
        oughta (dir </> fileName) prog

-- | Run an "Oughta"-based test written in a standalone Lua file
oughtaLua ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  TT.TestTree
oughtaLua dir fileName =
  let dropped = FilePath.dropExtension fileName
   in TTH.testCase dropped $ do
        content <- Text.IO.readFile (dir </> fileName)
        let prog = Oughta.plainLuaProgram fileName content
        oughta (dir </> fileName) prog

-- | Create a test from a file, depending on the extension
fileTest :: FilePath -> FilePath -> [TT.TestTree]
fileTest d f =
  case FilePath.takeExtension f of
    ".cbl" -> [oughtaSexp d f]
    ".elf" -> [oughtaElf d f]
    ".lua" -> [oughtaLua d f]
    _ -> []

-- | Recursively walk a directory tree, discovering tests
discoverTests :: FilePath -> IO TT.TestTree
discoverTests d = do
  entries <- Dir.listDirectory d
  fmap (TT.testGroup (FilePath.takeBaseName d) . List.concat) $
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
  tdata <- discoverTests "test-data"
  directedTests <- discoverTests "directed-test-data"
  TT.defaultMain $ TT.testGroup "symbolic executon tests" [tdata, directedTests]

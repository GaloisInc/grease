{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-
Test Oughta, using Oughta.

The test suite reads text files from @test-data/{fail,pass}/@ and runs Oughta
on them /twice/. All of the Lua code is embedded in the text files, on lines
that start with @#@. It first runs against the entire text file (specifically,
the non-@#@ lines). It then serializes the t'Ota.Result' and matches /that/
against the commands that start with @;@. This double-checking ensures that
directives match (and don't match) where expected, and that Oughta's output
is readable and correct (which is non-trivial, especially source span tracking).
-}
module Main (main) where

import Control.Monad qualified as Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.Function ((&))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text.IO
import Oughta qualified as Ota
import Prelude hiding (lines)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import Test.Tasty.HUnit qualified as TTH
import Test.Tasty qualified as TT

prelude :: ByteString
prelude = $(embedFile "test/test.lua")

test :: FilePath -> IO ()
test file = do
  content <- Text.IO.readFile file
  let comment = "# "
  let comment' = "; "

  -- White-out comments so that checks don't match themselves
  let isComment t = comment `Text.isPrefixOf` t ||  comment' `Text.isPrefixOf` t
  let rmComment l = if isComment l then Text.replicate (Text.length l) " " else l
  let clearComments out =
        Text.lines out &
        map rmComment &
        Text.unlines &
        Text.encodeUtf8 &
        Ota.Output

  let prog0 = Ota.fromLineComments file comment content
  let prog = Ota.addPrefix (Text.decodeUtf8Lenient prelude) prog0
  result <- Ota.check prog (clearComments content)
  TTH.assertBool file (not (Ota.resultNull result))

  let prog0' = Ota.fromLineComments file comment' content
  let prog' = Ota.addPrefix (Text.decodeUtf8Lenient prelude) prog0'
  let output'@(Ota.Output out) = clearComments (Ota.printResult result)
  BS.writeFile (FilePath.replaceExtension file "out") out
  Ota.check' prog' output'

discover :: FilePath -> IO [TT.TestTree]
discover dir = do
  entries <- map (dir </>) <$> Dir.listDirectory dir
  files <- Monad.filterM Dir.doesFileExist entries
  let txts = filter ((== ".txt") . FilePath.takeExtension) files
  pure (map (\file -> TTH.testCase file (test file)) txts)

main :: IO ()
main = do
  f <- discover "test-data/fail"
  p <- discover "test-data/pass"
  TT.defaultMain (TT.testGroup "Oughta tests" (f ++ p))

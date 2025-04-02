{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad qualified as Monad
import Data.Function ((&))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import FileCheck qualified as FC
import Prelude hiding (lines)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty.HUnit qualified as TTH
import Test.Tasty qualified as TT

discover :: FilePath -> IO [TT.TestTree]
discover dir = do
  entries <- map (dir </>) <$> Dir.listDirectory dir
  files <- Monad.filterM Dir.doesFileExist entries
  Monad.forM files $ \file -> do
    content <- Text.IO.readFile file
    pure $
      TTH.testCase file $ do
        let comment = "# "

        -- White-out comments so that `CHECK` lines don't match themselves
        let isComment = (comment `Text.isPrefixOf`)
        let rmComment l = if isComment l then Text.replicate (Text.length l) " " else l
        let clearComments out = 
              Text.lines out &
              map rmComment &
              Text.unlines &
              FC.Output

        let prefix = Nothing
        let (cmds, result) = FC.parseCommentsAndCheck prefix comment (Just file) content (clearComments content)
        let prefix' = Just (FC.Prefix "OUT")
        let output' = FC.printResult cmds result
        (_cmds, result') <- FC.parseCommentsAndCheck' prefix' comment (Just file) content (clearComments output')
        TTH.assertBool file (length (FC.resultMatches result') > 0)

main :: IO ()
main = do
  f <- discover "test-data/fail"
  p <- discover "test-data/pass"
  TT.defaultMain (TT.testGroup "FileCheck tests" (f ++ p))

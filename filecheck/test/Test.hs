{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad qualified as Monad
import Data.Either qualified as Either
import Data.Function ((&))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import FileCheck (Output(..), parseCommentsAndCheck)
import Prelude hiding (lines)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty.HUnit qualified as TTH
import Test.Tasty qualified as TT

discover :: Bool -> FilePath -> IO [TT.TestTree]
discover pass dir = do
  entries <- map (dir </>) <$> Dir.listDirectory dir
  files <- Monad.filterM Dir.doesFileExist entries
  Monad.forM files $ \file -> do
    content <- Text.IO.readFile file
    pure $
      TTH.testCase file $ do
        let prefix = Nothing
        let comment = "# "
        let isComment = (comment `Text.isPrefixOf`)
        let output =
              Text.lines content &
              filter (not . isComment) &
              Text.unlines &
              Output
        let result = parseCommentsAndCheck prefix comment (Just file) content output
        let check = if pass then Either.isRight else Either.isLeft
        TTH.assertBool file (check result)

main :: IO ()
main = do
  f <- discover False "test-data/fail"
  p <- discover True "test-data/pass"
  TT.defaultMain (TT.testGroup "FileCheck tests" (f ++ p))

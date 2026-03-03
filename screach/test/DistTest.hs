{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import DistCLI qualified as CLI
import Options.Applicative qualified as Opts

main :: IO ()
main =
  do
    conf <- Opts.execParser CLI.cli
    CLI.runDistTest conf

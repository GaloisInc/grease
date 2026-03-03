module Main (main) where

import Screach qualified
import Screach.Cli (cliFromArgs)
import Screach.Config qualified as Conf

main :: IO ()
main = do
  conf <- cliFromArgs
  Screach.runScreach conf (Screach.defaultLogAction (Conf.programConfig conf))

import Data.Maybe (mapMaybe)
import Data.String (IsString (fromString))
import DistCLI qualified as Dist
import Grease.Diagnostic.Severity qualified as GD
import Grease.Options qualified as GRS
import Screach.AnalysisLoc qualified as Screach
import Screach.Config (ProgramConfig (verbosity))
import Screach.Config qualified as Screach
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath
import Test.Tasty qualified as TT
import Test.Tasty.HUnit qualified as TTH
import Test.Tasty.Runners qualified as TT

fileToTest :: FilePath -> TT.TestTree
fileToTest fileName =
  let base = FilePath.dropExtension fileName
      conf =
        Dist.DisttestConfig
          { Dist.callgraph = base ++ "-cg.csv"
          , Dist.progConfig =
              Screach.ProgramConfig
                { Screach.confProgram = base ++ ".exe"
                , Screach.entryLoc =
                    Screach.EntryLoc
                      (Screach.AnalysisLocSymbol (fromString "foo"))
                      Nothing
                , verbosity = GD.Debug
                , Screach.stackArgumentSlots = GRS.ExtraStackSlots 0
                }
          }
   in TTH.testCase base $ Dist.runDistTest conf

findTests :: FilePath -> IO TT.TestTree
findTests d = do
  entries <- Dir.listDirectory d
  let tests =
        mapMaybe
          (\pth -> if FilePath.takeExtension pth == ".c" then Just $ fileToTest $ d FilePath.</> pth else Nothing)
          entries
  pure $ TT.TestGroup "distance tests" tests

main :: IO ()
main = TT.defaultMain =<< findTests "test-distance"

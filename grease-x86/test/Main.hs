{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.ByteString qualified as BS
import Data.ElfEdit qualified as Elf
import Data.FileEmbed (embedFileRelative)
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.ElfLoader qualified as ElfLoader
import Data.Macaw.Memory.LLVMJumpTableSizes qualified as JT
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Map.Strict qualified as Map
import Grease.Macaw.Arch.X86.LLVMJumpTableSizes qualified as X86JT
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit

pieFixture :: BS.ByteString
pieFixture =
  -- Force the parser to use the RELA addend instead of the target's contents.
  let rawFixture = $(embedFileRelative "test-data/llvm-jump-table-sizes-pie.elf")
      relocationTargetOffset = 0x2020
   in BS.take relocationTargetOffset rawFixture
        <> BS.replicate 8 0
        <> BS.drop (relocationTargetOffset + 8) rawFixture

main :: IO ()
main =
  Tasty.defaultMain $
    HUnit.testCase "parse PIE LLVM jump-table sizes" testPIEJumpTableSizes

testPIEJumpTableSizes :: HUnit.Assertion
testPIEJumpTableSizes =
  case Elf.decodeElfHeaderInfo pieFixture of
    Left err -> HUnit.assertFailure (show err)
    Right (Elf.SomeElf ehi) ->
      case Elf.headerClass (Elf.header ehi) of
        Elf.ELFCLASS32 -> HUnit.assertFailure "Expected an ELF64 fixture"
        Elf.ELFCLASS64 -> do
          let loadOpts = LC.LoadOptions{LC.loadOffset = Just 0x10000000}
          mem <-
            case ElfLoader.resolveElfContents loadOpts ehi of
              Left err -> HUnit.assertFailure err
              Right (_, loadedMem, _, _) -> pure loadedMem
          (warnings, sizes) <-
            case X86JT.x86_64LLVMJumpTableSizesFromElf loadOpts ehi mem of
              Left err -> HUnit.assertFailure (show err)
              Right result -> pure result
          HUnit.assertEqual "unresolvable addresses" [] warnings
          case Map.toList sizes of
            [(tableAddr, JT.JumpTableSize count)] -> do
              HUnit.assertEqual
                "relocated table address"
                (MM.MemAddr 0 (MM.memWord 0x10002000))
                (MM.segoffAddr tableAddr)
              HUnit.assertEqual "entry count" (MM.memWord 8) count
            entries ->
              HUnit.assertFailure $
                "Expected one jump-table size entry, got " ++ show entries

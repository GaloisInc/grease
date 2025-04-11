{-# LANGUAGE TemplateHaskell #-}

module FileCheck.Lua
  ( luaCode
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

luaCode :: ByteString
luaCode = $(embedFile "src/FileCheck/filecheck.lua")

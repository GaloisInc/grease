{-# LANGUAGE TemplateHaskell #-}

module Oughta.Lua
  ( luaCode
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

luaCode :: ByteString
luaCode = $(embedFile "src/Oughta/oughta.lua")

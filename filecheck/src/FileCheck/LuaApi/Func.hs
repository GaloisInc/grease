{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings#-}

-- | Functions in the FileCheck Lua API
module FileCheck.LuaApi.Func
  ( LuaFunc(..)
  , funcName
  , funcLuaName
  ) where

import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import HsLua qualified as Lua

-- | A function in the Lua API
data LuaFunc
  = Check
  deriving (Bounded, Enum)

-- | The name of a function in the Lua API
funcName :: LuaFunc -> Text
funcName =
  \case
    Check -> "check"

funcLuaName :: LuaFunc -> Lua.Name
funcLuaName = Lua.Name . Text.encodeUtf8 . funcName
{-# INLINABLE funcLuaName #-}

{-# LANGUAGE LambdaCase #-} do
{-# LANGUAGE OverloadedStrings #-}

module Grease.Requirement (
  Requirement (..),
  displayReq,
  RequirementParser,
  reqParser,
) where

import Control.Applicative (Alternative (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL

-- | A requirement that a software program should adhere to. This data type
-- enumerates all of the requirements that @grease@ explicitly supports checking
-- for. (There are a variety of other requirements that are implicitly checked
-- by ensuring that a program is memory-safe using @grease@'s memory model.)

-- NOTE: If you update this, make sure to also update the following:
--
-- - The documentation in @doc/requirements.md@
-- - The Ghidra plugin script's offering of requirements
data Requirement
  = -- | The requirement that code is never executed from memory that has write
    -- permissions.
    InText
  | -- | The requirement that code is not self-modifying.
    NoMprotect
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance Aeson.ToJSON Requirement
instance Aeson.ToJSONKey Requirement

instance PP.Pretty Requirement where
  pretty = PP.pretty . displayReq

displayReq :: Requirement -> Text
displayReq = \case
  InText -> "in-text"
  NoMprotect -> "no-mprotect"

-- | A @megaparsec@ parser type for 'Requirement's.
type RequirementParser = TM.Parsec Void Text

-- | Parse a symbol from 'TM.Tokens'.
symbol :: TM.Tokens Text -> RequirementParser Text
symbol = TMCL.symbol spaceConsumer

-- | A standard space consumer that does not support comments.
spaceConsumer :: RequirementParser ()
spaceConsumer = TMCL.space TMC.space1 empty empty

-- | Parse a 'Requirement'.
reqParser :: RequirementParser Requirement
reqParser = inText <|> noMprotect
 where
  inText = symbol "in-text" *> pure InText
  noMprotect = symbol "no-mprotect" *> pure NoMprotect

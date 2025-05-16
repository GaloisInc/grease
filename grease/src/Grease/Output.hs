{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Grease.Output
  ( Batch(..)
  , BatchBug(..)
  , BatchStatus(..)
  , CheckStatus(..)
  , FailedPredicate(..)
  , failedPredicateArgs
  , renderJSON
  ) where

import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import Grease.Bug qualified as Bug
import Grease.Heuristic.Result (CantRefine)
import Grease.Requirement (Requirement)
import Prettyprinter qualified as PP

type Location = Text

data FailedPredicate = FailedPredicate
  { _failedPredicateLocation :: Location
  , _failedPredicateMessage :: Text
    -- | Concrete arguments that make the bug occur.
  , _failedPredicateArgs :: [Aeson.Value]
    -- | Concrete shapes that make the bug occur.
  , _failedPredicateConcShapes :: Text
  } deriving (Show, Generic)
makeLenses ''FailedPredicate
instance Aeson.ToJSON FailedPredicate

-- | The result of checking a single assertion
data CheckStatus
  = CheckSuccess
  | CheckAssertionFailure [FailedPredicate]
  deriving (Show, Generic)
instance Aeson.ToJSON CheckStatus

instance PP.Pretty CheckStatus where
  pretty CheckSuccess = "All assertions passed"
  pretty (CheckAssertionFailure errs) = PP.vcat
    [ "One or more assertions failed:"
    , ""
    , PP.vcat $ errs <&> \e -> PP.vcat
      [ "At" PP.<+> PP.pretty (e ^. failedPredicateLocation)
      , PP.pretty (e ^. failedPredicateMessage)
      ]
    ]

data BatchBug
  = MkBatchBug
    { bugDesc :: Bug.BugInstance
      -- | Concrete arguments that make the bug occur.
    , bugArgs :: [Aeson.Value]
    -- | Concrete shapes that make the bug occur.
    , bugShapes :: Text
    }
  deriving (Show, Generic)
instance Aeson.ToJSON BatchBug

-- | The combined results of checking several assertions
data BatchStatus
  = BatchBug BatchBug
  | BatchCouldNotInfer (NE.NonEmpty FailedPredicate)
  | BatchItersExceeded
  | BatchChecks (Map Requirement CheckStatus)
  | BatchCantRefine CantRefine
  | BatchTimeout
  deriving (Show, Generic)
instance Aeson.ToJSON BatchStatus

instance PP.Pretty BatchStatus where
  pretty (BatchBug (MkBatchBug b _ shapes)) = PP.vcat $
    [ "Likely bug:" PP.<+> PP.pretty b
    ] ++
      if shapes == ""
      then []
      else [ ""
           , "Concretized arguments:"
           , PP.pretty shapes
           ]
  pretty (BatchChecks m) = mconcat $ Map.toList m <&> \(req, cs) -> mconcat
    [ "Checked" PP.<+> PP.pretty req <> ". Result:"
        PP.<+> PP.pretty cs
    ]
  pretty (BatchCouldNotInfer errs) = PP.vcat
    [ "Possible bug(s):"
    , ""
    , PP.vcat $ List.intersperse "" $ NE.toList errs <&> \e -> PP.vcat $
      [ "At" PP.<+> PP.pretty (e ^. failedPredicateLocation) <> ":"
      , PP.pretty (e ^. failedPredicateMessage)
      ] ++
      if e ^. failedPredicateConcShapes /= ""
      then [ "Concretized arguments:"
           , PP.pretty (e ^. failedPredicateConcShapes)
           ]
      else []
    ]
  pretty BatchItersExceeded =
    "Failed to infer precondition; maximum iterations exceeded"
  pretty (BatchCantRefine b) = PP.pretty b
  pretty BatchTimeout = "Exceeded timeout!"

-- | A 'BatchStatus' and any other information that is useful to report (e.g.,
-- for consumption by tools that use @grease@'s JSON output).
data Batch = Batch
  { batchStatus :: BatchStatus
    -- ^ The combined results of checking several assertions.
  , batchLoadOffset :: Word64
    -- ^ The load offset added to addresses that appear in @grease@'s output.
  } deriving (Show, Generic)
instance Aeson.ToJSON Batch

renderJSON :: Batch -> Text
renderJSON s = Text.decodeUtf8 . BSL.toStrict $ Aeson.encode s

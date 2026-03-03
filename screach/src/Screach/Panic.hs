{-# LANGUAGE TemplateHaskell #-}

module Screach.Panic (
  fromJust,
  panic,
  unimplemented,
) where

import Panic hiding (panic)
import Panic qualified

-- | Like 'fromJust', but panics in case of error.
fromJust :: String -> Maybe a -> a
fromJust msg val =
  case val of
    Nothing -> panic msg []
    Just val' -> val'

data Screach = Screach

-- | `panic` represents an error condition that should only
--   arise due to a programming error. It will exit the program
--   and print a message asking users to open a ticket.
panic ::
  HasCallStack =>
  -- | Short name of where the error occured
  String ->
  -- | More detailed description of the error
  [String] ->
  a
panic = Panic.panic Screach

unimplemented ::
  HasCallStack =>
  -- | Issue number
  String ->
  -- | Description
  String ->
  a
unimplemented issue msg =
  Panic.panic Screach ("Unimplemented: " ++ msg) ["See issue " ++ issue]

instance PanicComponent Screach where
  panicComponentName _ = "screach"
  panicComponentIssues _ = "https://github.com/GaloisInc/grease/-/issues"

  {-# NOINLINE panicComponentRevision #-}
  panicComponentRevision = $useGitRevision

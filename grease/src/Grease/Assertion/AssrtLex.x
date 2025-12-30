{
module Grease.Assertion.AssrtLex (alexScanTokens) where
import Grease.Assertion.Token qualified as Tokens
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  true                           {\_ -> Tokens.TokenBool(True)}
  false                          {\_ -> Tokens.TokenBool(False)}

{
  alexScanTokens :: String -> [Tokens.Token]
}
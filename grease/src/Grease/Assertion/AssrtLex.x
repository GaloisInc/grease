{
module Grease.Assertion.AssrtLex (alexMonadScan) where
import Grease.Assertion.Token qualified as Tokens
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$ident_end = [a-zA-z0-9\_]

@ident = $alpha+$ident_end*
@nat = $digit+
@hexdigit = [0-9A-Fa-f]
@hexnum = hexdigit+


tokens :-
  $white+ ;
  true {constToken $ Tokens.TokenBool True}
  -- false {const $ Tokens.TokenBool(False)}
  -- "(" {const Tokens.LParen}
  -- ")" {const Tokens.RParen}
  -- "*" {const Tokens.SepConj}
  -- "," {const Tokens.Comma}
  -- typeOf {const Tokens.TypeOf}
  -- zext {const Tokens.Zext}
  -- trunc {const Tokens.Trunc}
  -- "<" {const Tokens.Lt}
  -- "<=" {const Tokens.Lte}
  -- "<$" {const Tokens.Slt}
  -- "<=$" {const Tokens.Slte}
  -- "=" {const Tokens.Eq}
  -- "!" {const Tokens.Exclam}
  -- ite {const Tokens.Ite}
  -- "#" @ident {\s -> Tokens.ExistentialVar s}
  -- @ident {\s -> Tokens.ProgramVar s}
  -- "$" @ident {\s -> Tokens.LabelVar s}
  -- @hexnum ":" @nat {\s -> undefined}

{
constToken :: Tokens.Token -> AlexInput -> Int -> Alex Tokens.AnnotatedToken
constToken = undefined


alexEOF :: Alex Tokens.AnnotatedToken
alexEOF = do
  (pos, _, _ , _) <- alexGetInput
  let AlexPn _ line col = pos
  pure $ Tokens.AnnotatedToken line col Tokens.Eof
}
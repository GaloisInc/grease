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
  false {constToken $ Tokens.TokenBool False}
  "(" {constToken Tokens.LParen}
  ")" {constToken Tokens.RParen}
  "*" {constToken Tokens.SepConj}
  "," {constToken Tokens.Comma}
  typeOf {constToken Tokens.TypeOf}
  zext {constToken Tokens.Zext}
  trunc {constToken Tokens.Trunc}
  "<" {constToken Tokens.Lt}
  "<=" {constToken Tokens.Lte}
  "<$" {constToken Tokens.Slt}
  "<=$" {constToken Tokens.Slte}
  "=" {constToken Tokens.Eq}
  "!" {constToken Tokens.Exclam}
  ite {constToken Tokens.Ite}
  "#" @ident {tokenOf Tokens.ExistentialVar}
  @ident {tokenOf Tokens.ProgramVar}
  "$" @ident {tokenOf Tokens.LabelVar}
  @hexnum ":" @nat {tokenOf (\s -> Tokens.BvValue $ Tokens.parseBv s)}

{

tokenOf :: (String -> Tokens.Token) -> AlexInput -> Int -> Alex Tokens.AnnotatedToken
tokenOf f input@(_, _, _, s) sz =
  constToken (f s) input sz

constToken :: Tokens.Token -> AlexInput -> Int -> Alex Tokens.AnnotatedToken
constToken tok (pos, _, _, _) sz =
  let AlexPn _ line col = pos in
  pure $ Tokens.AnnotatedToken line col sz tok


alexEOF :: Alex Tokens.AnnotatedToken
alexEOF = do
  (pos, _, _ , _) <- alexGetInput
  let AlexPn _ line col = pos
  pure $ Tokens.AnnotatedToken line col 0 Tokens.Eof
}
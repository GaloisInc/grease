{
module Grease.Assertion.AssrtParse () where
import Grease.Assertion.Token qualified as Tokens
import Grease.Assertion.Ast qualified as Ast
}

%name assrts
%tokentype { Tokens.AnnotatedToken }
%error { parseError }


%token
      bool {  Tokens.AnnotatedBool _ _}
      sepconj { Tokens.AnnotatedToken Tokens.SepConj _}
%%

Expr : bool {let Tokens.AnnotatedBool b pos = $1 in Ast.annotatedExpr (Ast.ExprLit $ Ast.LitBool b) () pos}


SepForm :  Expr { Ast.Pure $1}
        | SepForm sepconj SepForm { Ast.SepConj $1 $3 }
{
parseError :: [Tokens.AnnotatedToken] -> a
parseError _ = error "Parse error"
}
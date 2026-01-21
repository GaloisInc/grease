{
module Grease.Assertion.AssrtParse () where
import Grease.Assertion.Token qualified as Tokens
import Grease.Assertion.Ast qualified as Ast
}

%name assrts
%tokentype { Tokens.Token }
%error { parseError }


%token
      bool { Tokens.TokenBool $$ }
      sepconj { Tokens.SepConj }
%%

Expr : bool {Ast.annotatedExpr (Ast.ExprLit $ Ast.LitBool $1) ()}


SepForm :  Expr { Ast.Pure $1}
        | SepForm sepconj SepForm { Ast.SepConj $1 $3 }
{
parseError :: [Tokens.Token] -> a
parseError _ = error "Parse error"
}
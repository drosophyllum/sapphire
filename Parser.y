{
module Parser where
import Lex
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    ';' { TSEMI }
    '.'  { TDOT }
    '->'  { TARROW }
    '*>'  { TCONV }
    int { TINT $$ }
    sym { TSYM $$ }

%%

Line :	Expr ';' Line { $1:$3}
    |   Expr ';'      { [$1] }

Expr : Neur '->' Neur {Arrow' $1 $3}
    |  Neur '*>' Neur {Conv' $1 $3 }


Neur : sym '.' int    {Layer' $1 $3}
    |  sym            {Neuron' $1 } 

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data AST = Arrow' ASTNeur ASTNeur  
         | Conv' ASTNeur ASTNeur 
         deriving Show

data ASTNeur = Layer' String Int
	|  Neuron' String 
	deriving Show
}

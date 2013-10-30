{
module Lex where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  "->"                          { \s -> TARROW}
  "*>"                          { \s -> TCONV}
  $digit+                       { \s -> TINT (read s) }
  \. 				{ \s -> TDOT}
  \;                            { \s -> TSEMI }
  $alpha [$alpha $digit \_ \']* { \s -> TSYM s }

{

data Token = TDOT
	   | TSEMI
           | TINT Int
           | TSYM String
	   | TARROW
	   | TCONV
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

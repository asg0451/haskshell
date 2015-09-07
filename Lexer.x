{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z\._]
$word  = [a-zA-Z\.\/_\-]

tokens :-

    $white+				;
    "#".*				;
    $digit+				{ TokInt . read }
    [\=]				{ const TokAssign }
    [\>]				{ const TokGT }
    [\<]				{ const TokLT }
    [\;]				{ const TokSemi }
    [\(]				{ const TokLP }
    [\)]				{ const TokRP }
    [\"]				{ const TokDQ }
    "=="				{ const TokEql }
    if					{ const TokIf }
    else				{ const TokElse }
    then				{ const TokThen }
    $word+                              { TokWord }


{
-- each type String -> Token

data Token
    = TokAssign
    | TokWord String
    | TokInt Int
    | TokSemi
    | TokLP
    | TokRP
    | TokDQ
    | TokGT
    | TokLT
    | TokEql
    | TokIf
    | TokElse
    | TokThen
      deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens

}

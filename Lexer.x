{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z\._]

tokens :-

    $white+				;
    "--".*				;
    $digit+				{ \s -> TokInt (read s) }
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
    $alpha+				{ \s -> TokWord s }

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

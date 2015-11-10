{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z\._]
$word  = [a-zA-Z\.\/_\-0-9]

tokens :-

    \"(\\.|[^\"])*\"                    { TokWord . stripQuotes }
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
    ">>"				{ const TokAppendOut }
    if					{ const TokIf }
    else				{ const TokElse }
    then				{ const TokThen }
    alias				{ const TokAlias }
    $word+                              { TokWord }
    [\|]                                { const TokPipe }

{
-- each type String -> Token

data Token
    = TokAssign
    | TokWord String
    | TokWordQuoted String
    | TokInt Int
    | TokSemi
    | TokLP
    | TokRP
    | TokDQ
    | TokGT
    | TokLT
    | TokAppendOut
    | TokEql
    | TokIf
    | TokElse
    | TokThen
    | TokAlias
    | TokPipe
      deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens

stripQuotes :: String -> String
stripQuotes s = reverse $ dropWhile (== '\"') $ reverse $ dropWhile (== '\"') s
}
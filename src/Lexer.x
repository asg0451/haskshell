{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z\._]
$word  = [a-zA-Z\.\/_\-0-9]
$newline = [\n]

tokens :-

    $newline+                           { const TokNL }
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
    [\*]$word*                          { TokGlobbedPath }
    $word*[\*]                          { TokGlobbedPath }
    [\$]$word+                          { TokVarRef }
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
    | TokNL
    | TokVarRef String
    | TokGlobbedPath String
      deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens

stripQuotes :: String -> String
stripQuotes = reverse . dropWhile (== '\"') . reverse . dropWhile (== '\"')

}

{  -- no indents here or else

module Parser (plex, Expression(..), Condition(..)) where

import Data.Char
}


-- parse :: [Token] -> T
%name parse Expr
%tokentype { Token }
%error { parseError }

%token
        '='	{ TokAssign }
        ';'	{ TokSemi }
        '('	{ TokLP }
        ')'	{ TokRP }
        '"'	{ TokDQ }
        word	{ TokWord $$}
        int     { TokInt $$}
        '>'     { TokGT }
        '<'     { TokLT }
        eql     { TokEql }
        if      { TokIf }
        else    { TokElse }
        then    { TokThen }

%left ';' -- precedence bitches
%left '='
-- %left else then    -- this line changes precedence of if stmts

%%  --- Productions

Expr: Commands                    { ComArgs (head $1) (tail $1) }
    | word '=' Expr               { Assign $1 $3 }
    | Expr ';' Expr               { Seq $1 $3 }
    | if Cond then Expr           { IfElse $2 $4 Nothing }
    | if Cond then Expr else Expr { IfElse $2 $4 (Just $6) }
    | '(' Expr ')'                { $2 }
    | Const                       { $1 }

Commands: word Commands           { $1 : $2 }  -- cant change this to const until
    | {- empty -}                 { [] }       -- change Comargs to take Literals

Cond: Expr '>' Expr               { Gt $1 $3 }
    | Expr '<' Expr               { Lt $1 $3 }
    | Expr eql Expr               { Eql $1 $3 }

Const: int                        { IntLiteral $1 }
    | '"' word '"'                { StrLiteral $2 }



{


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expression
    = ComArgs String [String]  -- if list empty, treat as var ref!
    | Assign String Expression
    | Seq Expression Expression
    | IfElse Condition Expression (Maybe Expression)  -- else clause optional
    | IntLiteral Int
    | StrLiteral String
    deriving Show


data Condition
    = Gt Expression Expression
    | Lt Expression Expression
    | Eql Expression Expression
      deriving Show

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
      deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c || c == '='  = lexVar (c:cs)  -- modified to allow ==
    | isDigit c = lexNum (c:cs)
lexer (';':cs) = TokSemi : lexer cs
lexer ('(':cs) = TokLP : lexer cs
lexer (')':cs) = TokRP : lexer cs
lexer ('"':cs) = TokDQ : lexer cs
lexer ('>':cs) = TokGT : lexer cs
lexer ('<':cs) = TokLT : lexer cs

lexNum cs = TokInt (read num) : lexer rest   -- ints only
    where (num,rest) = span isDigit cs

lexVar cs =
    case span (\c -> isAlpha c || c == '=' ) cs of  -- keywords go here
      ("if", rest) -> TokIf : lexer rest
      ("else", rest) -> TokElse  : lexer rest
      ("then", rest) -> TokThen  : lexer rest
      ("=", rest) -> TokAssign   : lexer rest
      ("==", rest) -> TokEql     : lexer rest
      (var, rest) -> TokWord var : lexer rest

plex :: String -> Expression
plex s = parse $ lexer s

}

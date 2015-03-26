{  -- no indents here or else

module Parser (plex, Expression(..), Condition(..)) where

import Prelude hiding (head, tail)
import Data.Char
import Lexer (lexer, Token(..))
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
tail :: [a] -> [a]
tail [] = []
tail l = drop 1 l

head [] = "not a real command, or a real solution"
head l = take 1 l !! 0

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

plex :: String -> Expression
plex = (parse . lexer)

}

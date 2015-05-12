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

Expr: Args                        { ComArgs (head $1) (tail $1) }
    | ConstStr '=' Const          { Assign (fromLit $1) $3 }
    | Expr ';' Expr               { Seq $1 $3 }
    | if Cond then Expr           { IfElse $2 $4 Nothing }
    | if Cond then Expr else Expr { IfElse $2 $4 (Just $6) }
    | '(' Expr ')'                { $2 }
    | {- empty -}                 { Empty }

Args: Const                       { [(fromLit $1)] }
    | Const Args                  { (fromLit $1) : $2 }

Cond: Const '>' Const             { Gt $1 $3 }    -- at the moment functionality is limited to consts
    | Const '<' Const             { Lt $1 $3 }
    | Const eql Const             { Eql $1 $3 }

Const: ConstInt                   { $1 }
     | ConstStr                   { $1 }

ConstInt: int                     { IntLiteral $1 }

ConstStr: '"' word '"'            { StrLiteral $2 }
        | word                    { StrLiteral $1 }

{

fromLit :: Expression -> String
fromLit e = case e of
                StrLiteral s -> s
                IntLiteral i -> show i

tail :: [a] -> [a]
tail [] = []
tail l = drop 1 l

head [] = "not a real command, or a real solution"
head l = take 1 l !! 0

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- todo seperate Expression into 2 datatypes: Expression and Constant

data Expression
    = ComArgs String [String]  -- if list empty, treat as var ref!
    | Assign String Expression
    | Seq Expression Expression
    | IfElse Condition Expression (Maybe Expression)  -- else clause optional
    | IntLiteral Int
    | StrLiteral String
    | Empty
    deriving Show

data Condition
    = Gt Expression Expression
    | Lt Expression Expression
    | Eql Expression Expression
      deriving Show

plex :: String -> Expression
plex = (parse . lexer)

}

{  -- no indents here or else

module Parser (plex, Expression(..), Condition(..)) where

import Data.Char
import Lexer (lexer, Token(..))
}


-- parse :: [Token] -> T
%name parse Line
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
        gt      { TokGT }
        lt      { TokLT }
        '>>'    { TokAppendOut }
        eql     { TokEql }
        if      { TokIf }
        else    { TokElse }
        then    { TokThen }
        alias   { TokAlias }
        pipe    { TokPipe }

%left ';' -- precedence bitches
%left '='
-- %left else then    -- this line changes precedence of if stmts

%%  --- Productions

Line: Expr gt ConstStr            { RedirectOut False $1 (fromLit $3) }
    | ConstStr lt Expr            { RedirectIn $3 (fromLit $1) }
    | Expr '>>' ConstStr          { RedirectOut True $1 (fromLit $3) }
    | Expr pipe Expr              { Pipe $1 $3 }
    | Expr                        { $1 }

Expr: Args                        { ComArgs (head $1) (tail $1) }
    | ConstStr '=' Const          { Assign (fromLit $1) $3 }
    | alias ConstStr '=' ConstStr { Alias (fromLit $2) (fromLit $4) }
    | Expr ';' Expr               { Seq $1 $3 }
    | if Cond then Expr           { IfElse $2 $4 Nothing }
    | if Cond then Expr else Expr { IfElse $2 $4 (Just $6) }
    | '(' Expr ')'                { $2 }
    | {- empty -}                 { Empty }

Args: Const                       { [(fromLit $1)] }
    | Const Args                  { (fromLit $1) : $2 }

Cond: Const gt Const              { Gt $1 $3 }    -- at the moment functionality is limited to consts
    | Const lt Const              { Lt $1 $3 }
    | Const eql Const             { Eql $1 $3 }

Const: ConstInt                   { $1 }
     | ConstStr                   { $1 }

ConstInt: int                     { IntLiteral $1 }

ConstStr: '"' '"'                 { StrLiteral ""}
        | '"' word '"'            { StrLiteral $2 }
        | word                    { StrLiteral $1 }

-- ConstStr: '"' '"'                 { StrLiteral "" }
--         | '"' Words '"'           { $2 }
--         | Words                   { $1 }

-- Words: Words word                 { StrLiteral ((fromLit $1) ++ $2) }
--      | word                       { StrLiteral $1 }


{

fromLit :: Expression -> String
fromLit e = case e of
                StrLiteral s -> s
                IntLiteral i -> show i

parseError :: [Token] -> a
parseError l = error $ "Parse error" ++ show l

data Expression
    = ComArgs String [String]  -- if list empty, treat as var ref!
    | Assign String Expression
    | Alias String String
    | Seq Expression Expression
    | IfElse Condition Expression (Maybe Expression)  -- else clause optional
    | RedirectOut Bool Expression String -- append or not
    | RedirectIn Expression String
    | Pipe Expression Expression
    | IntLiteral Int
    | StrLiteral String
    | Empty
    deriving (Show, Eq)

data Condition
    = Gt Expression Expression
    | Lt Expression Expression
    | Eql Expression Expression
    deriving (Show, Eq)

plex :: String -> Expression
plex = (parse . lexer)

}

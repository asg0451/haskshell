{  -- no indents here or else

module Parser (plex, Expression(..), Condition(..), StrOrRef(..)) where

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
        ref	{ TokVarRef $$}
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

Line: Expr gt ConstStr            { RedirectOut False $1 $3 }
    | ConstStr lt Expr            { RedirectIn $3 $1 }
    | Expr '>>' ConstStr          { RedirectOut True $1 $3 }
    | Expr pipe Expr              { Pipe $1 $3 }  -- TODO should go in Expr
    | Expr                        { $1 }

Expr: Args                        { ComArgs (head $1) (tail $1) }
    | ConstStr '=' SOR            { Assign $1 $3 }
    | alias ConstStr '=' ConstStr { Alias $2 $4 }
    | Expr ';' Expr               { Seq $1 $3 }
    | if Cond then Expr           { IfElse $2 $4 Nothing }
    | if Cond then Expr else Expr { IfElse $2 $4 (Just $6) }
    | '(' Expr ')'                { $2 }
    |                             {- empty -}                 { Empty }

Args: SOR                         { [$1] } -- last
    | SOR Args                    { $1 : $2 }

Cond: SOR gt SOR                  { Gt $1 $3 }
    | SOR lt SOR                  { Lt $1 $3 }
    | SOR eql SOR                 { Eql $1 $3 }

-- string or ref
SOR: ConstInt                     { Str $1 }
   | ConstStr                     { Str $1 }
   | ref                          { Ref $ drop 1 $ $1 }

-- below: type is String
ConstInt: int                     { show $1 }

ConstStr: '"' '"'                 { "" }
        | '"' word '"'            { $2 }
        | word                    { $1 }



{

-- TODO think about this
checkCom :: StrOrRef -> String
checkCom (Str s) = s
checkCom (Ref r) = error $ "parse error -- " ++ r ++ " is not a valid command"

parseError :: [Token] -> a
parseError l = error $ "Parse error" ++ show l

data Expression
    = ComArgs StrOrRef [StrOrRef]
    | Assign String StrOrRef -- until subshells are a thing
    | Alias String String
    | Seq Expression Expression
    | IfElse Condition Expression (Maybe Expression)  -- else clause optional
    | RedirectOut Bool Expression String -- append or not
    | RedirectIn Expression String
    | Pipe Expression Expression
    | Empty
    deriving (Show, Eq)

data StrOrRef
    = Str String
    | Ref String
    deriving (Show, Eq)

data Condition
    = Gt  StrOrRef StrOrRef
    | Lt  StrOrRef StrOrRef
    | Eql StrOrRef StrOrRef
    deriving (Show, Eq)

plex :: String -> Expression
plex = (parse . lexer)

}

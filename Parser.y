{  -- no indents here or else

-- TODO make expressions instances of traversable or foldable or something
--      also evaluate anything ever

import Data.Char
import Data.Monoid

import qualified System.Posix.Process as Proc
}

-- [var=expr] command opt(;) expr

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

%left ';' -- precedence bitches
%left '='

%%  --- Productions

Expr: Words                 { ComArgs (head $1) (tail $1) }
    | word '=' Expr         { Assign $1 $3 }
    | Expr ';' Expr         { Seq $1 $3 }
    | '(' Expr ')'          { $2 }
    | int                   { IntLiteral $1 }
    | '"' word '"'          { StrLiteral $2 }

Words: word Words   { $1 : $2 }
     | {- empty -}  { [] }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expression
    = ComArgs String [String]
    | Assign String Expression
    | Seq Expression Expression
    | IntLiteral Int
    | StrLiteral String
    deriving Show

data Token
    = TokAssign
    | TokWord String
    | TokInt Int
    | TokSemi
    | TokLP
    | TokRP
    | TokDQ
      deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokAssign : lexer cs
lexer (';':cs) = TokSemi : lexer cs
lexer ('(':cs) = TokLP : lexer cs
lexer (')':cs) = TokRP : lexer cs
lexer ('"':cs) = TokDQ : lexer cs

lexNum cs = TokInt (read num) : lexer rest   -- ints only
    where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlpha cs of  -- keywords go here
      (var, rest) -> TokWord var : lexer rest

main = do
  l <- getLine
  let ast = parse $ lexer l
  out <- eval ast
  putStrLn $ show out
  return ()

-- may have side effects, eg running programs
eval :: Expression -> IO Expression
eval v@(StrLiteral s) = return v
eval v@(IntLiteral i) = return v
eval v@(ComArgs c as) = Proc.executeFile c True as Nothing
-- setting 2nd arg to true means use PATH. will we have a PATH given to us without bash?
-- last arg is env. to use it properly, we need to implement StateT over IO and Maybe


}

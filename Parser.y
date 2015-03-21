{  -- no indents here or else

-- TODO make expressions instances of traversable or foldable or something
--      also evaluate anything ever
-- TODO switch from system.posix.process to system.process,
--      for return val and pipe support

import Data.Char
import Data.Monoid

import qualified System.Posix.Process as Proc
import qualified System.Environment as Env
import qualified Data.Maybe as M
import System.IO (hSetBuffering, stdin, BufferMode(..))
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
        '>'     { TokGT }
        if      { TokIf }
        else    { TokElse }
        then    { TokThen }

%left ';' -- precedence bitches
%left '='

%%  --- Productions

Expr: Words                       { ComArgs (head $1) (tail $1) }
    | word '=' Expr               { Assign $1 $3 }
    | Expr ';' Expr               { Seq $1 $3 }
    | if Cond then Expr           { IfElse $2 $4 Nothing }
    | if Cond then Expr else Expr { IfElse $2 $4 (Just $6) }
    | '(' Expr ')'                { $2 }
    | int                         { IntLiteral $1 }

Words: word Words                 { $1 : $2 }
     | '"' word '"' Words         { $2 : $4 }
     | {- empty -}                { [] }

Cond: Expr '>' Expr               { Gt $1 $3 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expression
    = ComArgs String [String]
    | Assign String Expression
    | Seq Expression Expression
    | IfElse Condition Expression (Maybe Expression)  -- else clause optional
    | IntLiteral Int
    | StrLiteral String
    | Nullary -- temporary solution
    deriving Show

data Condition
    = Gt Expression Expression
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
    | TokIf
    | TokElse
    | TokThen
      deriving Show

-- may have side effects, eg running programs
eval :: Expression -> IO Expression
eval v@(StrLiteral s) = return v
eval v@(IntLiteral i) = return v
eval v@(ComArgs c as) = do
  Proc.forkProcess $ Proc.executeFile c True as $ Nothing -- Just [("a", "fish")]
  return Nullary
-- setting 2nd arg to true means use PATH. will we have a PATH given to us without bash?
-- last arg is env. to use it properly, we need to implement StateT over IO and Maybe
-- using it will overwrite the env passed to it by caller

-- i made the decision here to just add a nullary expression type constructor
-- instead of wrapping expressions in maybe. This is a TEMPORARY solution
eval v@(IfElse c e1 e2) = do
  cond <- evalCond c
  return $ if cond
              then e1
              else maybe Nullary id e2

evalCond :: Condition -> IO Bool   --- impurity propagates
evalCond (Gt (StrLiteral s1) (StrLiteral s2)) =
    return $ (length s1) > (length s2) --for example
evalCond (Gt (IntLiteral a) (IntLiteral b))   =  return $ a > b
evalCond (Gt e1 e2) = do a <- eval e1
                         b <- eval e2
                         evalCond (Gt a b)


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
lexer ('>':cs) = TokGT : lexer cs

lexNum cs = TokInt (read num) : lexer rest   -- ints only
    where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlpha cs of  -- keywords go here
      ("if", rest) -> TokIf : lexer rest
      ("else", rest) -> TokElse : lexer rest
      ("then", rest) -> TokThen : lexer rest
      (var, rest) -> TokWord var : lexer rest

main = do
  hSetBuffering stdin LineBuffering
  l <- getLine
  let ast = parse $ lexer l
  out <- eval ast
  putStrLn $ show ast
  putStrLn $ show out
  return ()

}

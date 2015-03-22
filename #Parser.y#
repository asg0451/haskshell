{  -- no indents here or else



-- TODO switch from system.posix.process to system.process,
--      for return val and pipe support
--      also figure out how to
-- problem -- varRef vs single word commands. possible solution:
--              make if-stmts only contain exprs if theyre wrapped in brackets of some sort


import Data.Char
import Data.Monoid
import Control.Monad.State.Lazy

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

Expr: Commands                    { ComArgs (head $1) (tail $1) }
    | word '=' Expr               { Assign $1 $3 }
    | Expr ';' Expr               { Seq $1 $3 }
    | if Cond then Expr           { IfElse $2 $4 Nothing }
    | if Cond then Expr else Expr { IfElse $2 $4 (Just $6) }
    | '(' Expr ')'                { $2 }
    | int                         { IntLiteral $1 }
    | '"' word '"'                { StrLiteral $2 }

Commands: word Commands           { $1 : $2 }
     | {- empty -}                { [] }

Cond: Expr '>' Expr               { Gt $1 $3 }


{

type Eval = StateT Env IO -- partial
type Env  = [(String, String)]
data Val  = Str String
          | Null
            deriving Show
instance Monoid Val where
    mempty = Null
    (Str a) `mappend` (Str b) = Str $ unlines [a,b] -- only commands give output right?
    Null `mappend` Null = Null
    Null `mappend` (Str s) = Str s
    (Str s) `mappend` Null  = Str s

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

-- are all data types strings like in bash?
-- for now, making everything strings.
eval :: Expression -> Eval Val
eval expr = case expr of
              IntLiteral i -> return $ Str $ show i
              StrLiteral s -> return $ Str s
              Assign v (IntLiteral i) -> do
                        modify $ \s -> (v, show i) : s
                        return Null
              ComArgs c [] -> do     -- this is wrong. fails on one-word commands. see TODO's
                        env <- get   -- add state to env for process
                        case lookup c env of
                          Just val -> return $ Str val
                          Nothing  -> return Null
              ComArgs c as -> do
                        env <- get   -- add state to env for process
                        let args = map (\a -> maybe a (id) (lookup a env)) as
                        liftIO $ Proc.forkProcess $ Proc.executeFile c True args $ Just env
                        return Null
              Seq a b -> do ra <- eval a
                            rb <- eval b
                            return $ ra `mappend` rb
              IfElse c e1 (Just e2) -> do
                        b <- evalCond c
                        if b
                           then eval e1
                           else eval e2
              IfElse c e Nothing -> do
                        b <- evalCond c
                        if b
                           then eval e
                           else return Null

evalCond :: Condition -> Eval Bool
evalCond (Gt (IntLiteral a) (IntLiteral b)) = return $ a > b
evalCond (Gt (StrLiteral a) (StrLiteral b)) = return $ (length a) > (length b)
-- unfinished


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
  out <- runStateT (eval ast) []
  putStrLn $ show ast
  putStrLn $ show out
  return ()

}

-- TODO switch from system.posix.process to system.process,
--      for return val and pipe support
--      hopefully this will fix the order of execution of commands not being guaranteeable, i.e. not forked
-- problem -- varRef vs single word commands. possible solution:
--              make if-stmts only contain exprs if theyre wrapped in brackets of some sort
-- TODO make ComArgs take Literals instead of raw strings
-- TODO think about ambiguity, e.g. if 1 == 2 then echo hi ; a = 2
--       gets parsed as if .. then ( echo hi ; a = 2)
--       edit %left precedence accordingly


module Main where
import System.IO
import Parser
import Control.Monad.State.Lazy
import Data.Monoid
import qualified System.Posix.Process as Proc
import System.Console.Readline ( readline )

type Eval = StateT Env IO   -- partial type, takes an a value
type Env  = [(String, String)]
data Val  = Str String
          | Null
            deriving Show
instance Monoid Val where
    mempty = Null
    (Str a) `mappend` (Str b) = Str $ unlines [b,a] -- only commands give output right?
    (Str a) `mappend` Null = Str a
    Null    `mappend` x = x


-- are all data types strings like in bash?
-- for now, making everything strings.
eval :: Expression -> Eval Val
eval expr = case expr of
              IntLiteral i -> return $ Str $ show i
              StrLiteral s -> return $ Str s
              Assign v (IntLiteral i) -> do
                        modify $ \s -> (v, show i) : s
                        return $ Str $ show i
              Assign v (StrLiteral s) -> do
                        modify $ \state -> (v, s) : state
                        return $ Str $ s
              ComArgs c [] -> do     -- this is wrong. see TODO's
                        env <- get   -- add state to env for process
                        case lookup c env of
                          Just val -> return $ Str val
                          Nothing  -> return Null
              ComArgs c as -> do
                        env <- get   -- add state to env for process
                        let args = map (\a -> maybe a (id) (lookup a env)) as
                        liftIO $ Proc.forkProcess
                                   $ Proc.executeFile c True args $ Just env
                        return Null  -- return return value of command eventually
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

evalCond (Lt (IntLiteral a) (IntLiteral b)) = return $ a < b
evalCond (Lt (StrLiteral a) (StrLiteral b)) = return $ (length a) < (length b)

evalCond (Eql (IntLiteral a) (IntLiteral b)) = return $ a == b
evalCond (Eql (StrLiteral a) (StrLiteral b)) = return $ (length a) == (length b)



-- unfinished

main = do
--  hSetBuffering stdin LineBuffering
  l <- readline ">> "
  case l of
    Nothing  -> return ()
    Just l -> do
        let ast = plex l
        out <- runStateT (eval ast) [] -- evalStateT to suppress output of state
        putStrLn $ show ast
        putStrLn $ show out
        return ()

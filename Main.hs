{-# LANGUAGE TemplateHaskell #-}
module Main where
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Monoid
import           Parser
import           System.Console.Readline  (addHistory, readline)
import           System.Environment
import           System.Process           hiding (cwd, env, proc)
import qualified System.Process           as P (cwd, env)

type Env  = [(String, String)]
type Eval = StateT InternalState IO

data InternalState = InternalState { _env :: Env
                                   , _cwd :: String
                                   } deriving (Show, Read, Eq)
makeLenses ''InternalState


data Val  = Str String   -- could be a wrapper around Maybe String, but this type may not be final
          | Null
            deriving Show

instance Monoid Val where  -- todo: make it so that failed commands have some authority
    mempty = Null
    (Str a) `mappend` (Str b) = Str $ unlines [b,a] -- only commands give output right?
    (Str a) `mappend` Null = Str a
    Null    `mappend` x = x


isBuiltin :: String -> Bool
isBuiltin s = s `elem` ["cd"]

runBuiltin :: String -> [String] -> Eval Val
runBuiltin "cd" as = do
  if null as
    then  modify $ \st -> over cwd (const ".") st --wrong but lazy. TODO add $HOME to env
    else  modify $ \st -> over cwd (const $ head as) st
  return Null


runCom :: Env -> String -> [String] -> Eval Val
runCom e c as = do
  s <- get
  let dir = view cwd s
  (_, _, _, h) <- liftIO $ createProcess $ proc dir e c as
  retVal <- liftIO $ waitForProcess h
  return $ Str $ show retVal


eval :: Expression -> Eval Val
eval expr = case expr of
              IntLiteral i -> return $ Str $ show i
              StrLiteral s -> return $ Str s
              Assign v (IntLiteral i) -> do
                        modify $ \st -> over env (\e -> (v, show i) : e) st
                        return $ Str $ show i
              Assign v (StrLiteral s) -> do
                        modify $ \st -> over env (\e -> (v, s) : e) st
                        return $ Str s
              ComArgs c [] -> do     -- this is wrong. see TODO's
                        s <- get   -- add state to env for process
                        let environment = view env s
                        case lookup c environment of
                          Just val -> return $ Str val
                          Nothing  -> return Null
              -- TODO add support for pseudo-commands (internals) e.g. "cd"
              -- TODO add support for forking commands (background)
              ComArgs c as -> do
                        s <- get
                        let environment = view env s
                        let args = map (\a -> fromMaybe a (lookup a environment)) as
                        if isBuiltin c
                          then runBuiltin c args
                          else runCom environment c args
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
              Empty -> return Null


evalCond :: Condition -> Eval Bool
evalCond (Gt (IntLiteral a) (IntLiteral b)) = return $ a > b
evalCond (Gt (StrLiteral a) (StrLiteral b)) = return $ length a > length b

evalCond (Lt (IntLiteral a) (IntLiteral b)) = return $ a < b
evalCond (Lt (StrLiteral a) (StrLiteral b)) = return $ length a < length b

evalCond (Eql (IntLiteral a) (IntLiteral b)) = return $ a == b
evalCond (Eql (StrLiteral a) (StrLiteral b)) = return $ length a == length b
-- unfinished

main :: IO ()
main = do
  path <- getEnv "PATH"
  void $ iterateM_ (\prev -> do
                       line <- readline ">> "
                       case line of
                        Just l -> do addHistory l  -- for readline
                                     let ast = plex l
                                     print ast
                                     out <- runStateT (eval ast) (fromJust prev)
                                     print out
                                     let laststate = snd out
                                     return $ Just laststate
                        Nothing -> return Nothing
                   ) $ Just $ InternalState [("PATH", path)] "."

iterateM_ :: (Maybe a -> IO (Maybe a)) -> Maybe a -> IO (Maybe b)
iterateM_ f = g
    where g x = case x of
                  Nothing -> putStrLn "" >> return Nothing
                  Just z  -> f (Just z) >>= g


builtinCmd :: String -> [String] -> Eval Val
builtinCmd "cd" as = undefined




-- modified from function in System.Process to take an environment as an argument
proc :: FilePath -> Env -> FilePath -> [String] -> CreateProcess
proc wd env cmd args = CreateProcess { cmdspec = RawCommand cmd args
                                  , P.cwd = Just wd
                                  , P.env = Just env
                                  , std_in = Inherit
                                  , std_out = Inherit
                                  , std_err = Inherit
                                  , close_fds = False
                                  , create_group = False
                                  , delegate_ctlc = False
                                  }

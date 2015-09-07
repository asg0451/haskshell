{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where
import           Control.Concurrent       (myThreadId)
import qualified Control.Exception        as Ex
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List                (isPrefixOf)
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Monoid
import           Parser
import           System.Console.Readline  (addHistory, readline)
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.Posix.Signals     (Handler (..), installHandler,
                                           keyboardSignal, sigTSTP)
import           System.Process           hiding (cwd, env, proc)
import qualified System.Process           as P (cwd, env)
--------------------------------------------------------------------- Types

type Env  = [(String, String)]
type Eval = StateT InternalState IO
data InternalState = InternalState { _env :: Env
                                   , _cwd :: FilePath
                                   } deriving (Show, Read, Eq)
makeLenses ''InternalState

data Val  = Str String
          | Null
            deriving Show

instance Monoid Val where  -- todo: make it so that failed commands have some authority
    mempty = Null
    (Str a) `mappend` (Str b) = Str $ unlines [b,a] -- only commands give output right?
    (Str a) `mappend` Null = Str a
    Null    `mappend` x = x
------------------------------------------------------------------------

isBuiltin :: String -> Bool
isBuiltin s = s `elem` ["cd"]

-- TODO actual system for dealing with FilePaths
-- TODO look into how this is meant to be done: man chdir, ccal in unistd.h
runBuiltin :: String -> [String] -> Eval Val
runBuiltin "cd" as = do
  s <- get
  let wd = view cwd s
      arg = head as
  if | null as              -> modify $ \st -> set cwd wd st
     | arg == "."           -> return ()
     | arg == ".."          -> modify $ \st -> set cwd (takeDirectory wd) st
     | "-" `isPrefixOf` arg -> return ()
     | isRelative arg       -> modify $ \st -> over cwd (</> arg) st
     | otherwise            -> modify $ \st -> set cwd arg st
  return $ Str "ExitSuccess" -- TODO do some checking here

runCom :: Env -> String -> [String] -> Eval Val
runCom e c as = do
  s <- get
  let dir = view cwd s
  rval <- liftIO $ runProc $ proc_ dir e c as
  return $ fromMaybe (Str $ show $ ExitFailure 127) rval

runProc :: CreateProcess -> IO (Maybe Val)
runProc c = do hs <- Ex.catch (createProcess c >>= return . Just) handler
               case hs of
                Just (_, _, _, h) -> do
                  rval <- waitForProcess h
                  return $ Just $ Str $ show rval
                Nothing -> return Nothing
  where
    handler (e :: Ex.SomeException) = print "error caught: " >> print e >> return Nothing

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
              -- TODO add support for forking commands (background)
              ComArgs c as -> do
                s <- get
                let environment = view env s
                let args = map (\a -> fromMaybe a (lookup a environment)) as
                if isBuiltin c
                  then runBuiltin c args
                  else runCom environment c args
              Seq a b -> do
                ra <- eval a
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
  tid <- myThreadId
  {- Ex.throwTo tid Ex.UserInterrupt -}
  -- catch ctrl-c
  installHandler keyboardSignal (Catch (print "ctrl-c caught")) Nothing
  -- catch ctrl-z
  installHandler sigTSTP (Catch (print "ctrl-z" >> Ex.throwTo tid Ex.UserInterrupt)) Nothing
  path <- getEnv "PATH"
  home <- getEnv "HOME"
  let histFile = home ++ "/.HaskHistory"
  c <- readFile histFile
  let pastHistory = lines c
  mapM_ addHistory pastHistory
  void $ iterateM_ (\prev -> do
                       line <- readline ">> "
                       case line of
                        Just l -> do addHistory l  -- for readline
                                     when (not $ null l) $ appendFile histFile $ l ++ "\n"
                                     let ast = plex l
                                     print ast
                                     out <- runStateT (eval ast) (fromJust prev)
                                     print out
                                     let laststate = snd out
                                     return $ Just laststate
                        Nothing -> return Nothing
                   ) $ Just $ InternalState [("PATH", path)] home

iterateM_ :: (Maybe a -> IO (Maybe a)) -> Maybe a -> IO (Maybe b)
iterateM_ f = g
    where g x = case x of
                  Nothing -> putStrLn "" >> return Nothing
                  Just z  -> f (Just z) >>= g




-- modified from function in System.Process to take an environment as an argument
proc_ :: FilePath -> Env -> FilePath -> [String] -> CreateProcess
proc_ wd env cmd args = CreateProcess { cmdspec = RawCommand cmd args
                                     , P.cwd = Just wd
                                     , P.env = Just env
                                     , std_in = Inherit
                                     , std_out = Inherit
                                     , std_err = Inherit
                                     , close_fds = False
                                     , create_group = False
                                     , delegate_ctlc = False
-- below required from 'process-1.3.0.0'
                                     , detach_console = False     -- windows
                                     , create_new_console = False -- windows
                                     , new_session = False
                                     }

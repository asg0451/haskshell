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
import           System.Console.Readline
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.Posix.Signals     (Handler (..), installHandler,
                                           keyboardSignal, sigTSTP)
import           System.Process           hiding (cwd, env, proc)
import qualified System.Process           as P (cwd, env)

import qualified Data.Map.Lazy            as M
-- import           System.Posix.Env
--------------------------------------------------------------------- Types

type VarMap    = M.Map String String
type AliasMap  = M.Map String String
type Eval = StateT InternalState IO
data InternalState = InternalState { _vars      :: VarMap
                                   , _jobsTable :: M.Map () () -- not implemented yet
                                   , _aliases   :: AliasMap
                                   } deriving (Show, Read)
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

cwd :: IO FilePath
cwd = getCurrentDirectory

isBuiltin :: String -> Bool
isBuiltin s = s `elem` ["cd"]

runBuiltin :: String -> [String] -> Eval Val
runBuiltin "cd" as = do
  home <- liftIO $ getEnv "HOME"
  pwd <- liftIO $ cwd
  let arg = head as
  if | null as              -> do liftIO $ setCurrentDirectory home
                                  success
     | arg == "."           -> do success
     | arg == ".."          -> do liftIO $ setCurrentDirectory $ takeDirectory pwd
                                  success
     | "-" `isPrefixOf` arg -> do success
     | isRelative arg       -> do let dir = pwd </> arg
                                  dirp <- liftIO $ doesDirectoryExist dir
                                  if dirp
                                    then do liftIO $ setCurrentDirectory $ pwd </> arg
                                            success
                                    else return $ Str $ show $ ExitFailure 1
     | otherwise            -> do liftIO $  setCurrentDirectory $ arg
                                  success

success :: Eval Val
success = return $ Str $ show ExitSuccess

runCom :: String -> [String] -> Eval Val
runCom c as = do
  rval <- liftIO $ runProc $ proc_ c as
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

-- TODO use env for some vars, local thing for others
eval :: Expression -> Eval Val
eval expr = case expr of
              IntLiteral i -> return $ Str $ show i
              StrLiteral s -> return $ Str s
              Assign v (IntLiteral i) -> do
                modify $ \st -> over vars (M.insert v (show i)) st
                return $ Str $ show i
              Assign v (StrLiteral s) -> do
                modify $ \st -> over vars (M.insert v s) st
                return $ Str s
              ComArgs c [] -> do     -- this is wrong. see TODO's
                val <- liftIO $ lookupEnv c
                case val of
                 Just v -> return $ Str v
                 Nothing  -> return Null
              -- TODO add support for forking commands (background)
              ComArgs c as -> do
                e <- liftIO $ getEnvironment
                s <- get
                let args = map (\a -> fromMaybe a (lookup a e)) as
                    c' = M.findWithDefault c c $ view aliases s
                if isBuiltin c'
                  then runBuiltin c' args
                  else runCom  c' args
              Alias k v -> do modify $ over aliases $ M.insert k v
                              return $ Str v
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
                   ) $ Just $ InternalState M.empty M.empty M.empty

iterateM_ :: (Maybe a -> IO (Maybe a)) -> Maybe a -> IO (Maybe b)
iterateM_ f = g
    where g x = case x of
                  Nothing -> putStrLn "" >> return Nothing
                  Just z  -> f (Just z) >>= g




-- modified from function in System.Process to take an environment as an argument
proc_ :: FilePath -> [String] -> CreateProcess
proc_ cmd args = CreateProcess { cmdspec = RawCommand cmd args
                                     , P.cwd = Nothing
                                     , P.env = Nothing
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

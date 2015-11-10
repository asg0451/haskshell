{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- TODO use job table when dealing with processes
module Main where
import           Control.Concurrent       (myThreadId)
import qualified Control.Exception        as Ex
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Functor
import           Data.List                (isPrefixOf)
import           Data.List.Split
import qualified Data.Map.Lazy            as M
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Monoid
import           Parser
import           System.Console.Readline
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Signals     (Handler (..), installHandler,
                                           keyboardSignal, sigTSTP)
import           System.Process           hiding (cwd, env, proc)
import qualified System.Process           as P (cwd, env)
import           Types
--------------------------------------------------------------------- Types


------------------------------------------------------------------------

cwd :: IO FilePath
cwd = getCurrentDirectory

isBuiltin :: String -> Bool
isBuiltin = (`elem` ["cd"])

runBuiltin :: String -> [String] -> Eval Val
runBuiltin "cd" as = do
  home <- liftIO $ getEnv "HOME"
  pwd <- liftIO $ cwd
  let arg = head as
      dir = pwd </> arg
  dirp <- liftIO $ doesDirectoryExist dir
  if | not dirp             -> do liftIO $ print $ "not a directory: " ++ dir
                                  failure 1
     | null as              -> do liftIO $ setCurrentDirectory home
                                  success
     | arg == "."           -> do success
     | arg == ".."          -> do liftIO $ setCurrentDirectory $ takeDirectory pwd
                                  success
     | "-" `isPrefixOf` arg -> do success
     | isRelative arg       -> do liftIO $ setCurrentDirectory $ dir
                                  success
     | otherwise            -> do liftIO $ setCurrentDirectory $ arg
                                  success

success :: Eval Val
success = return $ ExitSuccess

failure :: Int -> Eval Val
failure = return . ExitFailure

---------- functions to run external commands

runCom :: String -> [String] -> Eval Val
runCom c as = do
  rval <- liftIO $ runProc $ proc_ c as
  return $ fromMaybe (ExitFailure 127) rval

-- e.g. ls > file
--               append?
-- bug: appends to beginning of file WHAT
-- todo WAT WAT
runComRedirOut :: Bool -> String -> [String] -> FilePath -> Eval Val
runComRedirOut app c as p = do
  h <- if app
       then liftIO $ openFile p WriteMode
       else liftIO $ openFile p AppendMode
  rval <- liftIO $ runProc $ (proc_ c as) {std_out = UseHandle h}
  return $ fromMaybe (ExitFailure 127) rval

-- e.g. ls < file
runComRedirIn :: String -> [String] -> FilePath -> Eval Val
runComRedirIn c as p = do
  h <- liftIO $ openFile p ReadMode
  rval <- liftIO $ runProc $ (proc_ c as) {std_in = UseHandle h}
  return $ fromMaybe (ExitFailure 127) rval

-- e.g. echo "hi" | cat
-- todo error check
runComsPiped :: String -> [String] -> String -> [String] -> Eval Val
runComsPiped c as d bs = do
 hs <- liftIO $ runProcReturningHandles $ (proc_ c as) {std_out = CreatePipe}
 let pout = fromJust $  view _2  $ fromJust hs
 hs' <- liftIO $ runProcReturningHandles $ (proc_ d bs) {std_in = UseHandle pout}
 liftIO $ waitForProcess (view _4 $ fromJust hs)
 liftIO $ waitForProcess (view _4 $ fromJust hs')
 return $ ExitSuccess


-- createProcess returns (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph)
runProc :: CreateProcess -> IO (Maybe Val)
runProc c = do hs <- Ex.catch (createProcess c >>= return . Just) handler
               case hs of
                Just (_, _, _, h) -> do
                  rval <- waitForProcess h
                  return $ Just $ rval
                Nothing -> return Nothing
  where
    handler (e :: Ex.SomeException) = putStr "error caught: " >> print e >> return Nothing


runProcReturningHandles :: CreateProcess -> IO (Maybe (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
runProcReturningHandles c = do
  hs <- Ex.catch (createProcess c >>= return . Just) handler
  case hs of
   Just hs -> do
     return $ Just $ hs
   Nothing -> return Nothing
  where
    handler (e :: Ex.SomeException) = print "error caught: " >> print e >> return Nothing

------------


eval :: Expression -> Eval Val
eval expr = case expr of
             RedirectOut app (ComArgs c as) f -> do
               e <- liftIO $ getEnvironment
               s <- get
               let c' = M.findWithDefault c c $ view aliases s
                   cPlusArgs = splitOn " " c'
                   c'' = head cPlusArgs
                   args = map (lookup2 e (view vars s)) $ as ++ tail cPlusArgs
               if isBuiltin c''
                 then runBuiltin c'' args
                 else runComRedirOut app c'' args f
             RedirectIn (ComArgs c as) f -> do
               e <- liftIO $ getEnvironment
               s <- get
               let c' = M.findWithDefault c c $ view aliases s
                   cPlusArgs = splitOn " " c'
                   c'' = head cPlusArgs
                   args = map (lookup2 e (view vars s)) $ as ++ tail cPlusArgs
               if isBuiltin c''
                 then runBuiltin c'' args -- todo
                 else runComRedirIn c'' args f

             RedirectOut a e f -> return $ ExitFailure 42 -- todo deal with this
             RedirectIn  e f -> return $ ExitFailure 42

             Pipe (ComArgs c as) (ComArgs d bs) -> do
               e <- liftIO $ getEnvironment
               s <- get
               let c' = M.findWithDefault c c $ view aliases s
                   cPlusArgs = splitOn " " c'
                   c'' = head cPlusArgs
                   args = map (lookup2 e (view vars s)) $ as ++ tail cPlusArgs
                   d' = M.findWithDefault d d $ view aliases s
                   dPlusArgs = splitOn " " d'
                   d'' = head dPlusArgs
                   dargs = map (lookup2 e (view vars s)) $ bs ++ tail dPlusArgs
               if isBuiltin c''
                 then runBuiltin c'' args -- todo
                 else runComsPiped c'' args d'' dargs

             IntLiteral i -> return ExitSuccess
             StrLiteral s -> return ExitSuccess
             Assign v (IntLiteral i) -> do
               modify $ \st -> over vars (M.insert v (show i)) st
               return ExitSuccess
             Assign v (StrLiteral s) -> do
               modify $ \st -> over vars (M.insert v s) st
               return ExitSuccess

             -- TODO add support for forking commands (background)
             ComArgs c as -> do
               e <- liftIO $ getEnvironment
               s <- get
               let c' = M.findWithDefault c c $ view aliases s
                   cPlusArgs = splitOn " " c'
                   c'' = head cPlusArgs
                   args = map (lookup2 e (view vars s)) $ as ++ tail cPlusArgs
               if isBuiltin c''
                 then runBuiltin c'' args
                 else runCom  c'' args
             Alias k v -> do modify $ over aliases $ M.insert k v
                             return ExitSuccess
             Seq a b -> do
               ra <- eval a
               case ra of
                 ExitSuccess -> do
                          rb <- eval b
                          return $ rb
                 ExitFailure _ -> do return ra
             IfElse c e1 (Just e2) -> do
               b <- evalCond c
               if b
                 then eval e1
                 else eval e2
             IfElse c e Nothing -> do
               b <- evalCond c
               if b
                 then eval e
                 else return ExitSuccess
             Empty -> return ExitSuccess
  where lookup2 a1 a2 e = case lookup e a1 of
                           Just r1 -> r1
                           Nothing -> case M.lookup e a2 of
                                       Just r2 -> r2
                                       Nothing -> e

-- TODO add ref'd var to parser, add to evalCond here
evalCond :: Condition -> Eval Bool
evalCond (Gt (IntLiteral a) (IntLiteral b)) = return $ a > b
evalCond (Gt (StrLiteral a) (StrLiteral b)) = return $ a > b
evalCond (Gt (StrLiteral a) (IntLiteral b)) = return $ a > (show b)
evalCond (Gt (IntLiteral a) (StrLiteral b)) = return $ (show a) > b

evalCond (Lt (IntLiteral a) (IntLiteral b)) = return $ a < b
evalCond (Lt (StrLiteral a) (StrLiteral b)) = return $ length a < length b
evalCond (Lt (StrLiteral a) (IntLiteral b)) = return $ a < (show b)
evalCond (Lt (IntLiteral a) (StrLiteral b)) = return $ (show a) < b

evalCond (Eql (IntLiteral a) (IntLiteral b)) = return $ a == b
evalCond (Eql (StrLiteral a) (StrLiteral b)) = return $ length a == length b
evalCond (Eql (StrLiteral a) (IntLiteral b)) = return $ a == (show b)
evalCond (Eql (IntLiteral a) (StrLiteral b)) = do -- no worky TODO
  e <- liftIO $ getEnvironment
  s <- get
  let vmap = view vars s
  let b' = M.findWithDefault b b vmap
  return $ show a == show b'

-- TODO fork to bg
-- TODO redirection
main :: IO ()
main = do
  tid <- myThreadId
  {- Ex.throwTo tid Ex.UserInterrupt -}
  -- catch ctrl-c
  installHandler keyboardSignal (Catch (putStr "ctrl-c caught")) Nothing
  -- catch ctrl-z. doesn't work.
  installHandler sigTSTP (Catch (print "ctrl-z" >> Ex.throwTo tid Ex.UserInterrupt)) Nothing
  home <- getEnv "HOME"
  let histFile = home ++ "/.HaskHistory"
  c <- readFile histFile
  let pastHistory = lines c
  mapM_ addHistory pastHistory
  void $ iterateM' (\prev -> do
                       line <- readline ">> "
                       case line of
                        Just l -> do addHistory l  -- for readline
                                     when (not $ null l) $ appendFile histFile $ l ++ "\n"
                                     astM <- cleanup $ plex l
                                     case astM of
                                      Just ast -> do
                                        putStrLn $ color $ show ast
                                        out <- runStateT (eval ast) (fromJust prev)
                                        print out
                                        let laststate = snd out
                                        return $ Just laststate
                                      Nothing -> do
                                        putStrLn $ "Lexical Error: " ++ l
                                        return $ prev
                        Nothing -> return Nothing
                   ) $ Just $ InternalState { _vars      = M.empty
                                            , _jobsTable = M.empty
                                            , _aliases   = M.fromList [("ls", "ls --color")]
                                            }

iterateM' :: (Maybe a -> IO (Maybe a)) -> Maybe a -> IO (Maybe b)
iterateM' f = g
    where g x = case x of
                  Nothing -> putStrLn "" >> return Nothing
                  Just z  -> f (Just z) >>= g

cleanup :: a -> IO (Maybe a)
cleanup x =  Ex.catch (x `seq` return (Just x)) handler
    where
          handler (ex :: Ex.ErrorCall) = return Nothing

color :: String -> String
color s = "\x1b[32m" ++ s ++ "\x1b[0m"


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
                                     -- , detach_console = False     -- windows
                                     -- , create_new_console = False -- windows
                                     -- , new_session = False
                                     }

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- TODO use job table when dealing with processes
-- TODO parsing for more than one pipe

module Haskshell where

import           Control.Concurrent                  (myThreadId)
import qualified Control.Exception                   as Ex
import           Control.Lens                        hiding (Empty)
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Functor
import           Data.List                           (isPrefixOf)
import           Data.List.Split
import qualified Data.Map.Lazy                       as M
import           Data.Maybe                          (fromJust, fromMaybe)
import           Data.Monoid
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Glob                (glob)
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Signals                (Handler (..),
                                                      installHandler,
                                                      keyboardSignal, sigTSTP)
import           System.Process                      hiding (cwd, env, proc)
import qualified System.Process                      as P (cwd, env)

import           System.Console.Haskeline
import           System.Console.Haskeline.Completion

import           Safe

import           Parser
import           Types
---------------------------------------------------------------------

io ::  MonadIO m => IO a -> m a
io = liftIO

---------------------------------------------------------------------


cwd :: IO FilePath
cwd = getCurrentDirectory

isBuiltin :: String -> Bool
isBuiltin = (`elem` ["cd"])

runBuiltin :: String -> [String] -> Eval Val
runBuiltin "cd" as = do
  home <- io $ getEnv "HOME"
  pwd <- io $ cwd
  let arg = headDef "" as
      dir = pwd </> arg
  dirp <- io $ doesDirectoryExist dir
  if | "-" `isPrefixOf` arg -> do success
     | not dirp             -> do io $ print $ "not a directory: " ++ dir
                                  failure 1
     | null as              -> do io $ setCurrentDirectory home
                                  success
     | arg == "."           -> do success
     | arg == ".."          -> do io $ setCurrentDirectory $ takeDirectory pwd
                                  success
     | isRelative arg       -> do io $ setCurrentDirectory $ dir
                                  success
     | otherwise            -> do io $ setCurrentDirectory $ arg
                                  success

success :: Eval Val
success = return ExitSuccess

failure :: Int -> Eval Val
failure = return . ExitFailure

---------- functions to run external commands

runCom :: String -> [String] -> Eval Val
runCom c as = do
  rval <- io $ runProc $ proc_ c as
  return $ fromMaybe (ExitFailure 127) rval

-- e.g. ls > file
--               append?
-- bug: appends to beginning of file WHAT
-- todo WAT WAT
runComRedirOut :: Bool -> String -> [String] -> FilePath -> Eval Val
runComRedirOut app c as p = do
  h <- if app
       then io $ openFile p AppendMode
       else io $ openFile p WriteMode
  rval <- io $ runProc $ (proc_ c as) {std_out = UseHandle h}
  return $ fromMaybe (ExitFailure 127) rval

-- e.g. ls < file
runComRedirIn :: String -> [String] -> FilePath -> Eval Val
runComRedirIn c as p = do
  h <- io $ openFile p ReadMode
  rval <- io $ runProc $ (proc_ c as) {std_in = UseHandle h}
  return $ fromMaybe (ExitFailure 127) rval

-- e.g. echo "hi" | cat
-- todo error check
runComsPiped :: String -> [String] -> String -> [String] -> Eval Val
runComsPiped c as d bs = do
 hs <- io $ runProcReturningHandles $ (proc_ c as) {std_out = CreatePipe}
 let pout = fromJust $  view _2  $ fromJust hs
 hs' <- io $ runProcReturningHandles $ (proc_ d bs) {std_in = UseHandle pout}
 io $ waitForProcess (view _4 $ fromJust hs)
 io $ waitForProcess (view _4 $ fromJust hs')
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
   Just hs' -> do
     return $ Just $ hs'
   Nothing -> return Nothing
  where
    handler (e :: Ex.SomeException) = print "error caught: " >> print e >> return Nothing

------------


eval :: Expression -> Eval Val
eval expr = case expr of
             RedirectOut app (ComArgs c as) f -> do
                        (com, args) <- evalComArgs c as
                        if isBuiltin com
                        then runBuiltin com args
                        else runComRedirOut app com args f
             RedirectIn (ComArgs c as) f -> do
                        (com, args) <- evalComArgs c as
                        if isBuiltin com
                        then runBuiltin com args -- todo
                        else runComRedirIn com args f

             RedirectOut a e f -> return $ ExitFailure 42 -- todo deal with this
             RedirectIn  e f -> return $ ExitFailure 42

             Pipe (ComArgs c as) (ComArgs d bs) -> do
                        (com, args) <- evalComArgs c as
                        (com2, args2) <- evalComArgs d bs
                        if isBuiltin com
                        then runBuiltin com args -- todo
                        else runComsPiped com args com2 args2

             Assign v (Str s) -> do
               modify $ \st -> over vars (M.insert v s) st
               return ExitSuccess
             Assign v (Ref r) -> do
               modify $ \st -> over vars (M.insert v r) st -- TODO
               return $ ExitFailure 42 -- not implemented

             -- TODO add support for forking commands (background)
             ComArgs c as -> do
                        (com, args) <- evalComArgs c as
                        if isBuiltin com
                        then runBuiltin com args
                        else runCom  com args

             Alias k v -> do modify $ over aliases $ M.insert k v
                             return ExitSuccess

             Seq a b -> do
               ra <- eval a
               rb <- eval b
               return rb

-- more appropriate for &&
             -- Seq a b -> do
             --   ra <- eval a
             --   case ra of
             --     ExitSuccess -> do
             --              rb <- eval b
             --              return $ rb
             --     ExitFailure _ -> do return ra
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

lookup2 a1 a2 e = case M.lookup e a2 of -- call with env, then vars
                    Just r1 -> r1
                    Nothing -> case lookup e a1 of
                                 Just r2 -> r2
                                 Nothing -> "" -- empty if not found. like bash

evalComArgs :: StrOrRef -> [StrOrRef] -> Eval (String, [String])
evalComArgs c as = do
  e <- io $ getEnvironment
  s <- get
  let c' = case c of
              Str str -> str
              Ref ref -> lookup2 e (view vars s) ref
      c'' = M.findWithDefault c' c' $ view aliases s
      cPlusArgs = splitOn " " c''
      c''' = head cPlusArgs
      args = tail cPlusArgs ++ map
                  (\a -> case a of
                           Str str -> str
                           Ref r -> lookup2 e (view vars s) r
                  ) as
  args_globbed <- io $ mapM (\a -> if elem '*' a
                                       then glob a
                                       else return [a]) args
  return (c''', concat args_globbed)

-- TODO FRANK when eval strliteral -> check state if string is stored as value first
-- example: a = 5
-- if 5 == a then echo hi
-- a is seen as a string, instead check if a is in state first

-- everything is either a string or a reference denoted by "$" -- just swapped to this

evalCond :: Condition -> Eval Bool
evalCond (Gt (Ref r) (Str s))    = compareRefToStr r s GT
evalCond (Gt (Str s) (Ref r))    = compareStrToRef s r GT
evalCond (Gt (Ref r1) (Ref r2))  = compareRefToRef r1 r2 GT
evalCond (Gt (Str s1) (Str s2))  = compareStrToStr s1 s2 GT

evalCond (Lt (Ref r) (Str s))    = compareRefToStr r s LT
evalCond (Lt (Str s) (Ref r))    = compareStrToRef s r LT
evalCond (Lt (Ref r1) (Ref r2))  = compareRefToRef r1 r2 LT
evalCond (Lt (Str s1) (Str s2))  = compareStrToStr s1 s2 LT

evalCond (Eql (Ref r) (Str s))   = compareRefToStr r s EQ
evalCond (Eql (Str s) (Ref r))   = compareStrToRef s r EQ
evalCond (Eql (Ref r1) (Ref r2)) = compareRefToRef r1 r2 EQ
evalCond (Eql (Str s1) (Str s2)) = compareStrToStr s1 s2 EQ

compareRefToRef :: String -> String -> Ordering -> Eval Bool
compareRefToRef r1 r2 ord = do
  e <- io $ getEnvironment
  s <- get
  let val1 = lookup2 e (view vars s) r1
      val2 = lookup2 e (view vars s) r2
  compareStrToStr val1 val2 ord

compareRefToStr :: String -> String -> Ordering -> Eval Bool
compareRefToStr r s ord = do
  e <- io $ getEnvironment
  st <- get
  let val1 = lookup2 e (view vars st) r
  compareStrToStr val1 s ord

compareStrToRef :: String -> String -> Ordering -> Eval Bool
compareStrToRef s r ord = compareRefToStr r s $ invert ord
    where invert EQ = EQ
          invert LT = GT
          invert GT = LT

-- really just need to check for Double or String
-- switch to type: String -> String -> Ordering -> Eval Bool
compareStrToStr :: String -> String -> Ordering -> Eval Bool
compareStrToStr s1 s2 ord
  | (firstarg == Nothing) || (secondarg == Nothing) = return $ (s1 `compare` s2) == ord
  | otherwise = return $ (fromJust $ (fmap compare firstarg) <*> secondarg) == ord
  where
    firstarg  = readDoubleOrString s1
    secondarg = readDoubleOrString s2

readDoubleOrString :: String -> Maybe Double
readDoubleOrString s = readMay s :: Maybe Double

-- TODO fork to bg
-- TODO redirection
hmain :: IO ()
hmain = do
  home <- getEnv "HOME"
  setupHandlers
  void $ runInputT (haskelineSettings home) $ iterateM' iteration (Just initialState)
      where iteration :: (Maybe InternalState) -> InputT IO (Maybe InternalState)
            iteration prev = do
              line <- getInputLine ">> "
              case line of
                Just l -> do astM <- io $ cleanup $ plex l
                             case astM of
                               Just ast -> do
                                         outputStrLn $ green $ show ast
                                         out <- io $ runStateT (eval ast) (fromJust prev)
                                         outputStrLn $ purple $ show out
                                         let laststate = snd out
                                         return $ Just laststate
                               Nothing -> do
                                         outputStrLn $ "Lexical Error: " ++ l
                                         return $ prev
                Nothing -> return Nothing


initialState =
       InternalState { _vars      = M.empty
                     , _jobsTable = M.empty
                     , _aliases   = M.fromList [("ls", "ls --color")]
                     }

haskelineSettings h = setComplete (completeFilename) $ defaultSettings {
                        autoAddHistory = True
                      , historyFile = Just $ h </> ".HaskHistory"
                    }

setupHandlers :: IO ()
setupHandlers = void $ do
  tid <- myThreadId
  {- Ex.throwTo tid Ex.UserInterrupt -}
  -- catch ctrl-c
  installHandler keyboardSignal (Catch (putStr "ctrl-c caught")) Nothing
  -- catch ctrl-z. doesn't work.
  installHandler sigTSTP (Catch (print "ctrl-z" >> Ex.throwTo tid Ex.UserInterrupt)) Nothing


iterateM' :: (MonadIO m) => (Maybe a -> m (Maybe a)) -> Maybe a -> m (Maybe b)
iterateM' f = g
    where g x = case x of
                  Nothing -> io (putStrLn "") >> return Nothing
                  Just z  -> f (Just z) >>= g

cleanup :: a -> IO (Maybe a)
cleanup x =  Ex.catch (x `seq` return (Just x)) handler
    where
          handler (ex :: Ex.ErrorCall) = return Nothing

green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"

purple :: String -> String
purple s = "\x1b[35m" ++ s ++ "\x1b[0m"


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

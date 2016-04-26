module TestUtils where

import           Control.Monad.State.Lazy
import           Haskshell                as H
import           Lexer                    as L
import           Parser                   as P
import           Types                    as T

import           System.Process


runHaskshell :: String -> IO (Val, InternalState)
runHaskshell s = runStateT (eval ast) initialState
    where ast = plex s

runHaskshellState :: String -> InternalState -> IO (Val, InternalState)
runHaskshellState s state = runStateT (eval ast) state
    where ast = plex s


runHaskshellAst :: Expression -> IO (Val, InternalState)
runHaskshellAst e = runStateT (eval e) initialState

runHaskshellAstState :: Expression -> InternalState -> IO (Val, InternalState)
runHaskshellAstState e state = runStateT (eval e) state

cmd :: String -> IO String
cmd c = readCreateProcess (shell c) ""

void :: Monad m => m a -> m ()
void m = m >> return ()

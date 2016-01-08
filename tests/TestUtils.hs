module TestUtils where

import           Control.Monad.State.Lazy
import           Haskshell                as H
import           Lexer                    as L
import           Parser                   as P
import           Types                    as T


runHaskshell :: String -> IO (Val, InternalState)
runHaskshell s = runStateT (eval ast) initialState
    where ast = plex s

runHaskshellState :: String -> InternalState -> IO (Val, InternalState)
runHaskshellState s state = runStateT (eval ast) state
    where ast = plex s

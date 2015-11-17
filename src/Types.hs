{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


---- Note that Expression and Condition types are stored in the Parser code.


module Types where
import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.Map.Lazy            as M
import           System.Exit
import           System.Process           hiding (cwd, env, proc)
import qualified System.Process           as P (cwd, env)

    ----
--------------------------------------------------------------------------

type Val = ExitCode

data Process = Process { _argv      :: [String] -- to pass to exec
                       , _pid       :: ProcessHandle
                       , _completed :: Bool
                       , _stopped   :: Bool
                       , _status    :: ExitCode
 }
instance Show Process where
  show (Process a p c s st) = show a ++ show c ++ show s ++ show st

makeLenses ''Process

data Job = Job { _command  :: String -- what is this
               , _procs    :: [Process]
               , _gid      :: String -- have to think about this
               , _notified :: Bool -- has user been notified of stopped job
--               , _tmodes :: -- saved terminal modes termios idk
               , _stdin    :: StdStream
               , _stdout   :: StdStream
               , _stderr   :: StdStream
}
instance Show Job where
  show (Job c p g n i o e) = show c ++ show p ++ show g ++ show n

makeLenses ''Job

type JobId = Int
type JobsTable = M.Map JobId Job -- switch to linked list?


type VarMap    = M.Map String String
type AliasMap  = M.Map String String
type Eval = StateT InternalState IO
data InternalState = InternalState { _vars      :: VarMap
                                   , _jobsTable :: JobsTable
                                   , _aliases   :: AliasMap
                                   } deriving (Show)
makeLenses ''InternalState

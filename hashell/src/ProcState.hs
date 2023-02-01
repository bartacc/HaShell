module ProcState(ProcState(..), isFinished, processStatusToProcState) where

import System.Posix.Signals ( Signal )
import System.Exit (ExitCode)
import System.Posix (ProcessStatus (..))

data ProcState = 
      RUNNING 
    | STOPPED Signal 
    | TERMINATED Signal 
    | EXITED ExitCode
    deriving Show

data ProcStateName = RUNNING_ | STOPPED_ | TERMINATED_ | EXITED_ deriving (Eq)

procStateName :: ProcState -> ProcStateName 
procStateName s = 
    case s of 
        RUNNING -> RUNNING_
        STOPPED _ -> STOPPED_
        TERMINATED _ -> TERMINATED_
        EXITED _ -> EXITED_

instance Eq ProcState where
  s1 == s2 = procStateName s1 == procStateName s2
  s1 /= s2 = procStateName s1 /= procStateName s2

isFinished :: ProcState -> Bool
isFinished procState =
    case procState of
        EXITED _ -> True
        TERMINATED _ -> True
        _ -> False

-- Custom ProcState is needed, because we need a RUNNING state,
-- to know that a process has been continued (with SIGCONT)
processStatusToProcState :: ProcessStatus -> ProcState
processStatusToProcState processStatus =
    case processStatus of
        Exited code -> EXITED code
        Terminated sig _ -> TERMINATED sig
        Stopped sig -> STOPPED sig
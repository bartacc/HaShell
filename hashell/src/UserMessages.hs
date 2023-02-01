module UserMessages(printMessage, printMessageLn, printSuspended, printRunningInBackground, printContinued, getMessageBasedOnState) where

import JobsState ( JobID )    
import System.Console.Isocline (termWrite, termWriteLn)
import System.Exit (ExitCode)
import System.Posix (Signal)
import ProcState (ProcState(..))

printMessageLn :: String -> IO ()
printMessageLn = termWriteLn

printMessage :: String -> IO ()
printMessage = termWrite

printSuspended :: JobID -> String -> IO ()
printSuspended jobID cmdString = printMessage $ suspended jobID cmdString

printRunningInBackground :: JobID -> String -> IO ()
printRunningInBackground jobID cmdString = printMessage $ runningInBackground jobID cmdString

printContinued :: JobID -> String -> IO ()
printContinued jobId cmdString = printMessage $ continued jobId cmdString

getMessageBasedOnState :: JobID -> String -> ProcState -> String
getMessageBasedOnState jobID cmdString jobState =
    case jobState of 
        RUNNING -> runningInBackground jobID cmdString
        STOPPED _ -> suspended jobID cmdString
        TERMINATED sig -> killed jobID cmdString sig
        EXITED exitCode -> exited jobID cmdString exitCode


suspended :: JobID -> String -> String
suspended jobID cmdString = showJobIDAndAction jobID cmdString "suspended" ++ "\n"

runningInBackground :: JobID -> String -> String
runningInBackground jobID cmdString = showJobIDAndAction jobID cmdString "running" ++ "\n"

killed :: JobID -> String -> Signal -> String
killed jobID cmdString signal = showJobIDAndAction jobID cmdString "killed" ++ " by signal " ++ show signal ++ "\n"

exited :: JobID -> String -> ExitCode -> String
exited jobID cmdString exitCode = showJobIDAndAction jobID cmdString "exited" ++ " with exit status " ++ show exitCode ++ "\n"

continued :: JobID -> String -> String
continued jobId cmdString = showJobIDAndAction jobId cmdString "continue" ++ "\n"

showJobIDAndAction :: JobID -> String -> String -> String
showJobIDAndAction jobID cmdString action = "[" ++ show jobID ++ "] " ++ action ++ " '" ++ cmdString ++ "'"

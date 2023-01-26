module UserMessages(suspended, runningInBackground) where

import JobsState ( Job (cmdString), JobID )    
import System.Console.Isocline (termWriteLn)



suspended :: JobID -> Job -> IO ()
suspended jobID job = 
    termWriteLn $ "[" ++ show jobID ++ "]" ++ " suspended '" ++ cmdString job ++ "'"

runningInBackground :: JobID -> String -> IO ()
runningInBackground jobID cmdString =
    termWriteLn $ "[" ++ show jobID ++ "]" ++ " running '" ++ cmdString ++ "'"
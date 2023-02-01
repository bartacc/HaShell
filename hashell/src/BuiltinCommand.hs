module BuiltinCommand(isBuiltinCmd, runBuiltinCmd) where
    
import ProcessToRun (ProcessToRun (cmdName, args))
import JobsState (JobsState, JobID)
import Control.Monad.Trans.State (StateT)
import Jobs (watchJobs, killJob, sigchldMask, resumeJob)
import UserMessages (printMessage)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Read (readMaybe)
import System.Posix (getSignalMask, blockSignals, setSignalMask, SignalSet)

builtinCommands :: [(String, [String] -> StateT JobsState IO ())]
builtinCommands = [
    ("jobs", doJobs),
    ("kill", doKill),
    ("bg", doBg),
    ("fg", doFg)
    ]

isBuiltinCmd :: ProcessToRun -> Bool
isBuiltinCmd procToRun = 
    case lookup (cmdName procToRun) builtinCommands of
        Nothing -> False
        Just _ -> True

runBuiltinCmd :: ProcessToRun -> StateT JobsState IO ()
runBuiltinCmd procToRun = 
    do
        let commandName = cmdName procToRun
        case lookup commandName builtinCommands of
            Nothing -> error $ "Invalid builtin command " ++ commandName
            Just func -> func (args procToRun)

doBg :: [String] -> StateT JobsState IO ()
doBg args 
    | null args = 
        runCommand $ resumeJob True (-1)
    | length args == 1 = 
        readIntArgAndRunCommand args "Invalid job number for bg argument" $ resumeJob True
    | otherwise = 
        liftIO $ printMessage "Usage: bg | bg [job number]"

doFg :: [String] -> StateT JobsState IO ()
doFg args 
    | null args = 
        runCommand $ resumeJob False (-1)
    | length args == 1 = 
        readIntArgAndRunCommand args "Invalid job number for fg argument" $ resumeJob False 
    | otherwise = 
        liftIO $ printMessage "Usage: fg | fg [job number]"


doJobs :: [String] -> StateT JobsState IO ()
doJobs _ = watchJobs False

doKill :: [String] -> StateT JobsState IO ()
doKill args =
    if length args /= 1 then
        liftIO $ printMessage "Usage: kill [job number]\n"
    else 
        readIntArgAndRunCommand args "Invalid job number for the kill argument\n" killJob


readIntArgAndRunCommand :: [String] -> String -> (JobID -> SignalSet -> StateT JobsState IO ()) -> StateT JobsState IO ()
readIntArgAndRunCommand args onFailureMsg onSuccess =
    let maybeJobId = readMaybe $ head args :: Maybe Int in
    case maybeJobId of 
        Nothing -> liftIO $ printMessage onFailureMsg
        Just jobId -> runCommand $ onSuccess jobId



runCommand :: (SignalSet -> StateT JobsState IO ()) -> StateT JobsState IO ()
runCommand cmdToRun = do
    oldSigMask <- liftIO getSignalMask
    liftIO $ blockSignals sigchldMask            

    cmdToRun oldSigMask

    liftIO $ setSignalMask oldSigMask
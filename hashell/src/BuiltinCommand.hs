module BuiltinCommand(isBuiltinCmd, runBuiltinCmd) where
    
import ProcessToRun (ProcessToRun (cmdName, args))
import JobsState (JobsState)
import Control.Monad.Trans.State (StateT)
import Jobs (watchJobs, killJob, sigchldMask)
import UserMessages (printMessage)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Read (readMaybe)
import System.Posix (getSignalMask, blockSignals, setSignalMask)

builtinCommands :: [(String, [String] -> StateT JobsState IO ())]
builtinCommands = [
    ("jobs", doJobs),
    ("kill", doKill)
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

doJobs :: [String] -> StateT JobsState IO ()
doJobs _ = watchJobs False

doKill :: [String] -> StateT JobsState IO ()
doKill args =
    if length args /= 1 then
        liftIO $ printMessage "Usage: kill [job number]\n"
    else 
        let maybeJobId = readMaybe $ head args :: Maybe Int in
        case maybeJobId of 
            Nothing -> liftIO $ printMessage "Invalid job number for the kill argument\n"
            Just jobId -> do
                oldSigMask <- liftIO getSignalMask
                liftIO $ blockSignals sigchldMask

                killJob jobId

                liftIO $ setSignalMask oldSigMask



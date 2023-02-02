module RunCommand (run) where

import Parser
    ( Command(..), CommandToRun(..), CommandWithArgs )
import JobsState ( JobsState (jobs), addJob, addProc, Job (pgid), JobID )

import Control.Monad.Trans.State.Lazy ( StateT, execStateT, get, put )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Posix (Fd, closeFd, executeFile, installHandler, sigCHLD, Handler (Default), sigTSTP, sigTTIN, sigTTOU, sigQUIT, dupTo, setSignalMask, blockSignals, forkProcess, ProcessID, getSignalMask, createProcessGroupFor, SignalSet, addSignal, emptySignalSet, backgroundWrite, stdInput, stdOutput, getProcessID, createPipe, ProcessGroupID, joinProcessGroup, setProcessGroupIDOf)
import System.Console.Isocline (termWriteLn)
import Control.Exception (try, SomeException)
import Control.Monad
import ProcessToRun (ProcessToRun(..), createProcessToRun, overWriteInputOutputFdsIfNonNegative)
import BuiltinCommand (isBuiltinCmd, runBuiltinCmd)
import Jobs (sigchldMask, setTerminalPgid, monitorJob)
import qualified UserMessages
import DebugLogger (debug)
import qualified Data.IntMap as IntMap
import UserMessages (printMessageLn)
import qualified Data.List as List
import Data.Maybe (fromJust)


run :: CommandToRun -> StateT JobsState IO ()
-- Execute internal command within shell's process or execute external command
-- in a subprocess. External command can be run in the background.
run (CommandToRun (SingleCommand cmdWithArgs) commandName isBg) =
        do 
                procToRun <- liftIO $ createProcessToRun cmdWithArgs isBg

                liftIO $ debug $ "Run single command - " ++ show procToRun

                if isBuiltinCmd procToRun then
                        runBuiltinCmd procToRun
                else do
                        mask <- liftIO getSignalMask
                        liftIO $ blockSignals sigchldMask 

                        state <- get
                        childPid <- liftIO $ forkProcess $ childAfterFork state procToRun mask
                        parentAfterFork childPid procToRun mask

                        liftIO $ setSignalMask mask

        where 
                childAfterFork :: JobsState -> ProcessToRun -> SignalSet -> IO ()
                childAfterFork state procToRun origSigMask = do 
                        -- We call createProcessGroupFor and setTerminalPgid in parent and in child to avoid race.
                        childPid <- getProcessID
                        pgid <- createProcessGroupFor childPid

                        debug $ "In child after fork: childPid=" ++ show childPid ++ " childPgid=" ++ show pgid 

                        unless isBg $ do 
                                blockSignals (addSignal backgroundWrite emptySignalSet)
                                setTerminalPgid state pgid
                                setSignalMask origSigMask

                                debug "In child after fork: setTerminalPgid finished"

                        execBuiltinOrExternalProcess procToRun state

                parentAfterFork :: ProcessID -> ProcessToRun -> SignalSet -> StateT JobsState IO ()
                parentAfterFork childPid procToRun origSigMask =
                        do
                        -- TODO:
                        -- if (setpgid(pid, pid) < 0 && errno != EACCES) {
                        --       /* If errno == EACCESS, then the child process is already after exec and
                        --        * it has pgid set */
                        --       unix_error("Setpgid error");
                        --     }
                        childPgid <- liftIO $ createProcessGroupFor childPid

                        liftIO $ debug $ "In parent after fork: childPid=" ++ show childPid ++ " childPgid=" ++ show childPgid 

                        -- Close files which serve as stdin/stdout redirection for child.
                        liftIO $ maybeCloseFd (inputFD procToRun)
                        liftIO $ maybeCloseFd (outputFD procToRun)

                        state <- get
                        let (stateWithJob, jobId) = addJob state childPgid isBg commandName 
                        let stateWithProc = addProc stateWithJob jobId childPid
                        put stateWithProc

                        if isBg then
                                liftIO $ UserMessages.printRunningInBackground jobId commandName
                        else
                                monitorJob origSigMask


run (CommandToRun (PipelineCommand cmdsWithArgs) commandName isBg) =
        do 
                mask <- liftIO getSignalMask
                liftIO $ blockSignals sigchldMask 

                cmdWithPipes <- liftIO createCmdWithPipes
                newJobId <- startJobWithPipes cmdWithPipes

                if isBg then
                        liftIO $ UserMessages.printRunningInBackground newJobId commandName
                else
                        monitorJob mask

                liftIO $ setSignalMask mask
        where 
                startJobWithPipes :: [(Int, (CommandWithArgs, (Fd, Fd)))] -> StateT JobsState IO JobID
                startJobWithPipes cmdWithPipes = do 
                        maybeJobIdx <- foldM
                                (startProcessInPipeline cmdWithPipes)
                                Nothing
                                cmdWithPipes

                        case maybeJobIdx of 
                                Nothing -> error "Couldn't start the job"
                                Just jobIdx -> return jobIdx
                

                startProcessInPipeline :: [(Int, (CommandWithArgs, (Fd, Fd)))] -> Maybe JobID -> (Int, (CommandWithArgs, (Fd, Fd))) -> StateT JobsState IO (Maybe JobID)
                startProcessInPipeline cmdWithPipes maybeJobIdx (idx, (cmdWithArgs, (_, fdWrite))) = do
                        initialState <- get

                        -- Get the read end of the pipe from the previous entry in cmdWithPipes
                        let fdRead = 
                                if idx > 0 then 
                                        fst $ snd $ fromJust $ List.lookup (idx - 1) cmdWithPipes
                                else -1

                        -- Create ProcessToRun and fill it with pipe fds
                        procToRun <- liftIO $ createProcessToRun cmdWithArgs isBg
                        let procToRunWithPipes = overWriteInputOutputFdsIfNonNegative procToRun fdRead fdWrite

                        -- If this is the first process in pipeline we will create a new pgid. 
                        -- Otherwise use the pgid from previous processes in job.
                        let maybeChildPgid = 
                                case maybeJobIdx of
                                        Nothing -> Nothing
                                        Just jobIdx -> 
                                                let job = jobs initialState IntMap.! jobIdx in
                                                Just $ pgid job

                        -- Fork and exec child 
                        childPid <- liftIO $ forkProcess $ childAfterFork initialState procToRunWithPipes maybeChildPgid
                        liftIO $ parentAfterFork childPid maybeChildPgid procToRunWithPipes

                        -- Update JobsState. Add job if this is the first process in pipeline and add process to this job.
                        let (stateWithJob, jobIdx) = 
                                case maybeJobIdx of
                                        Nothing -> addJob initialState childPid isBg commandName
                                        Just existingJobIdx -> (initialState, existingJobIdx)
                        let stateWithAddedProc = addProc stateWithJob jobIdx childPid
                        put stateWithAddedProc

                        return $ Just jobIdx
                

                createCmdWithPipes :: IO [(Int, (CommandWithArgs, (Fd, Fd)))]
                createCmdWithPipes = do
                        pipes <- liftIO $ mapM 
                                (const createPipe) $
                                take (length cmdsWithArgs - 1) cmdsWithArgs
                        let pipesWithLastElem = pipes ++ [(-1, -1)]
                        let cmdAndPipeZip = zip cmdsWithArgs pipesWithLastElem
                        let cmdWithPipesMap = zip [0 .. length cmdsWithArgs - 1] cmdAndPipeZip
                        return cmdWithPipesMap

                childAfterFork :: JobsState -> ProcessToRun -> Maybe ProcessGroupID -> IO ()
                childAfterFork state procToRun maybeChildPgid = do
                        childPid <- getProcessID
                        setChildPgid childPid maybeChildPgid

                        execBuiltinOrExternalProcess procToRun state

                parentAfterFork :: ProcessID -> Maybe ProcessGroupID -> ProcessToRun -> IO ()
                parentAfterFork childPid maybeChildPgid procToRun = do
                        setChildPgid childPid maybeChildPgid
                        
                        -- Close files or pipes which serve as stdin/stdout redirection for child.
                        maybeCloseFd (inputFD procToRun)
                        maybeCloseFd (outputFD procToRun)

                setChildPgid :: ProcessID -> Maybe ProcessGroupID -> IO ()
                setChildPgid childPid maybeChildPgid = do
                        -- If this is the first process in pipeline, create new pgid for it.
                        -- Otherwise use the provided pgid.
                        case maybeChildPgid of
                                Nothing -> do
                                        _ <- createProcessGroupFor childPid
                                        return ()
                                Just childPgid -> setProcessGroupIDOf childPid childPgid



-- Close file descriptor if it was open, or ignore if it's already closed
maybeCloseFd :: Fd -> IO ()
maybeCloseFd fd =
        when (fd /= -1) $ do 
                _ <- try $ closeFd fd :: IO (Either SomeException ())
                return ()



revertSignalsToDefault :: IO ()
revertSignalsToDefault = 
        do
                _ <- installHandler sigCHLD Default Nothing
                debug "Reset sigCHLD to default."

                _ <- installHandler sigTSTP Default Nothing
                debug "Reset sigTSTP to default."

                _ <- installHandler sigTTIN Default Nothing
                debug "Reset sigTTIN to default."

                _ <- installHandler sigTTOU Default Nothing
                debug "Reset sigTTOU to default."

                _ <- installHandler sigQUIT Default Nothing
                debug "Reset sigQUIT to default."

                -- Resetting sigINT sometimes causes the program to stop.
                -- Not sure why that's happening.
                -- _ <- installHandler sigINT Default Nothing
                -- debug "Reset sigINT to default."

                return ()

duplicateRedirFds :: Fd -> Fd -> IO ()
duplicateRedirFds newInputFd newOutputFd =
        do
                maybeDuplicateFd newInputFd stdInput
                maybeDuplicateFd newOutputFd stdOutput

        where
                maybeDuplicateFd :: Fd -> Fd -> IO ()
                maybeDuplicateFd newFd origFd =
                        when (newFd /= -1) $
                        do
                                _ <- dupTo newFd origFd
                                maybeCloseFd newFd

execExternalProcess :: ProcessToRun -> IO ()
execExternalProcess procToRun =
        do
                revertSignalsToDefault
                duplicateRedirFds (inputFD procToRun) (outputFD procToRun)
                executeFile (ProcessToRun.cmdName procToRun) True (ProcessToRun.args procToRun) Nothing


execBuiltinOrExternalProcess :: ProcessToRun -> JobsState -> IO ()
execBuiltinOrExternalProcess procToRun jobsState =
        do
                debug "In child before revertSignalsToDefault"
                revertSignalsToDefault
                debug "In child after revertSignalsToDefault"

                debug "In child before duplicateRedirFds"
                duplicateRedirFds (inputFD procToRun) (outputFD procToRun)
                debug "In child after duplicateRedirFds"

                if isBuiltinCmd procToRun then do
                        -- This bulitin command is run in a forked child process, so we don't want to allow the child to modify parent's state.
                        -- That's why we run this computation in a new StateT monad, with state being the state of the parent (shell) at the the time of calling fork().
                        _ <- execStateT (runBuiltinCmd procToRun) jobsState
                        return ()
                        
                else 
                        executeFile (ProcessToRun.cmdName procToRun) True (ProcessToRun.args procToRun) Nothing
module RunCommand (run) where

import Parser
    ( Command(..), CommandToRun(..) )
import JobsState ( JobsState, addJob, addProc )

import Control.Monad.Trans.State.Lazy ( StateT, execStateT, get, put )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Posix (Fd, closeFd, executeFile, installHandler, sigCHLD, Handler (Default), sigTSTP, sigTTIN, sigTTOU, sigQUIT, dupTo, setSignalMask, blockSignals, forkProcess, ProcessID, getSignalMask, createProcessGroupFor, SignalSet, addSignal, emptySignalSet, backgroundWrite, stdInput, stdOutput, getProcessID)
import System.Console.Isocline (termWriteLn)
import Control.Exception (try, SomeException)
import Control.Monad
import ProcessToRun (ProcessToRun(..), createProcessToRun)
import BuiltinCommand (isBuiltinCmd, runBuiltinCmd)
import Jobs (sigchldMask, setTerminalPgid, monitorJob)
import qualified UserMessages
import DebugLogger (debug)


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
                                liftIO $ UserMessages.runningInBackground jobId commandName
                        else
                                monitorJob origSigMask


run (CommandToRun (PipelineCommand cmdsWithArgs) commandName isBg) =
        do 
                liftIO $ termWriteLn "Command Done"


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
module Jobs(initJobs, monitorJob, sigchldMask, setTerminalPgid) where

import JobsState (JobsState (..), Job (pendingSignalsForProcessGroup, pgid), ProcessUpdateInfo, updateState, fgIdx, getFgJob, getFgJobState, moveFGJobToBG)
import qualified UserMessages


import Control.Concurrent.STM
    ( STM, atomically, newTChan, tryReadTChan, writeTChan, TChan )
import qualified Data.IntMap as IntMap
import System.Posix
import ProcState ( processStatusToProcState, ProcState (..) )
import Control.Monad.IO.Class ( MonadIO(liftIO) ) 
import Control.Monad ( guard )
import Control.Concurrent ( yield )
import Control.Monad.Trans.State (StateT, put, get)
import System.Console.Isocline (termWriteLn)

sigchldMask :: SignalSet
sigchldMask = addSignal processStatusChanged emptySignalSet

-- TODO:
-- resumeJob - after sending SIGCONT update job state to RUNNING. 
--             Can't do that from sigchildHandler, because unix Haskell library doesn't have bindings
--             for WCONTINUED flag in waitpid() :/
-- killJob
-- watchJobs 
-- shutdownJobs

setTerminalPgid :: JobsState -> ProcessGroupID -> IO ()
setTerminalPgid state pgid =
    do
        setTerminalProcessGroupID (terminalFd state) pgid

setTerminalPgidToShell :: JobsState -> IO ()
setTerminalPgidToShell state = 
    do
        pgrp <- getProcessGroupID
        setTerminalPgid state pgrp

monitorJob :: SignalSet -> StateT JobsState IO ()
monitorJob signalSet =
    do
        initialState <- get
        liftIO $ setTerminalProcessGroupID (terminalFd initialState) (pgid $ getFgJob initialState)
        waitForFgJobToFinishOrStop

    where 
        waitForFgJobToFinishOrStop :: StateT JobsState IO ()
        waitForFgJobToFinishOrStop = 
            do
                waitForSigchld signalSet
                state <- get 
                let fgJobState = getFgJobState state in
                    case fgJobState of
                        STOPPED _ -> do
                            let (newState, bgJobId) = moveFGJobToBG state
                            put newState
                            liftIO $ UserMessages.suspended bgJobId $ getFgJob newState
                            liftIO $ setTerminalPgidToShell newState
                        EXITED _ -> liftIO $ setTerminalPgidToShell state
                        TERMINATED _ -> liftIO $ setTerminalPgidToShell state
                        _ -> waitForFgJobToFinishOrStop
                            
                


sigchldHandler :: TChan ProcessUpdateInfo -> IO ()
sigchldHandler stmChan =
    do
        maybeProcState <- getAnyProcessStatus False True 
        case maybeProcState of
            Nothing -> return ()
            Just (pid, procState) -> do
                atomically $ writeTChan stmChan (pid, processStatusToProcState procState)
                sigchldHandler stmChan


initJobs :: IO JobsState
initJobs = do
    stmChan <- atomically newTChan

    --    Block SIGINT for the duration of `sigchldHandler`
    --    in case `sigintHandler` does something crazy like `longjmp`.
    let signalMaskToBlock = addSignal keyboardSignal emptySignalSet
    _ <- installHandler processStatusChanged (Catch $ sigchldHandler stmChan) (Just signalMaskToBlock)

    -- Assume we're running in interactive mode, so move us to foreground.
    -- Duplicate terminal fd, but do not leak it to subprocesses that execve.
    isTerminal <- queryTerminal stdInput
    guard isTerminal 
    ttyFd <- dup stdInput
    setFdOption ttyFd CloseOnExec True

    -- Take control of the terminal.
    pgid <- getProcessGroupID
    setTerminalProcessGroupID ttyFd pgid

    -- Return initial state of shell
    return JobsState {
        jobs = IntMap.empty,
        terminalFd = ttyFd,
        stmChannel = stmChan
    }

-- Uses sigSuspend to wait for a sigChld.
-- Then it updates the current state based on the information from sigchldHandler()
waitForSigchld :: SignalSet -> StateT JobsState IO ()
waitForSigchld signalSet =
    do
        liftIO $ awaitSignal $ Just signalSet -- Wait for a sigChld and install old mask (where sigChld is unblocked)
        liftIO yield -- Give a chance for the sighchldHandler to run

        initialState <- get
        let stmChan = stmChannel initialState
        stateAfterChannelRead <- liftIO $ atomically $ readChannelAndUpdateState stmChan initialState
        liftIO $ sendPendingSignals $ jobs stateAfterChannelRead
        let finalState = removePendingSignalsFromState stateAfterChannelRead
        put finalState

    where
        readChannelAndUpdateState :: TChan ProcessUpdateInfo -> JobsState -> STM JobsState
        readChannelAndUpdateState chan initialState = do
            maybeUpdateInfo <- tryReadTChan chan
            case maybeUpdateInfo of 
                Nothing -> return initialState
                Just updateInfo ->
                    readChannelAndUpdateState chan $ updateState initialState updateInfo

        sendPendingSignals :: IntMap.IntMap Job -> IO ()
        sendPendingSignals =
            mapM_ 
            (\job -> 
                mapM_ 
                (\sig -> signalProcessGroup sig $ pgid job) 
                $ pendingSignalsForProcessGroup job)

        removePendingSignalsFromState :: JobsState -> JobsState
        removePendingSignalsFromState initialState =
            initialState {
                jobs = 
                    IntMap.map 
                    (\job -> job {pendingSignalsForProcessGroup = []}) 
                    $ jobs initialState
            }
module Jobs(initJobs, monitorJob, sigchldMask, setTerminalPgid) where

import JobsState (JobsState (..), Job (pendingSignalsForProcessGroup, pgid), ProcessUpdateInfo, updateState, getFgJob, getFgJobState, moveFGJobToBG)
import qualified UserMessages


import Control.Concurrent.STM
    ( STM, atomically, newTChan, tryReadTChan, writeTChan, TChan, readTChan )
import qualified Data.IntMap as IntMap
import System.Posix
import ProcState ( processStatusToProcState, ProcState (..) )
import Control.Monad.IO.Class ( MonadIO(liftIO) ) 
import Control.Monad ( guard, when )
import Control.Concurrent ( yield )
import Control.Monad.Trans.State (StateT, put, get)
import DebugLogger (debug)
import Control.Exception (try)
import Foreign.C (getErrno, eCHILD, throwErrno)

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
        let ttyFd = terminalFd state
        isValidTerminalFd <- queryTerminal ttyFd
        guard isValidTerminalFd
        setTerminalProcessGroupID ttyFd pgid
        debug $ "After setTerminalProcessGroupID ttyFd=" ++ show (terminalFd state) ++ " pgid=" ++ show pgid ++ " isValidTtyFd=" ++ show isValidTerminalFd

setTerminalPgidToShell :: JobsState -> IO ()
setTerminalPgidToShell state = 
    do
        pgrp <- getProcessGroupID
        setTerminalPgid state pgrp

monitorJob :: SignalSet -> StateT JobsState IO ()
monitorJob signalSet =
    do
        initialState <- get

        liftIO $ debug $ "In monitorJob. JobsState = " ++ show initialState

        liftIO $ setTerminalPgid initialState (pgid $ getFgJob initialState)

        liftIO $ debug "In monitorJob. After setTerminalProcessGroupID"
        waitForFgJobToFinishOrStop

    where 
        waitForFgJobToFinishOrStop :: StateT JobsState IO ()
        waitForFgJobToFinishOrStop = 
            do
                liftIO $ debug $ "monitorJob before waitForSigchld"
                waitForSigchld signalSet
                liftIO $ debug $ "monitorJob after waitForSigchld"

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
                            
                


-- Gets all processes which changed stated (using waitpid()) and 
-- sends messages about it to TChan.
sigchldHandler :: TChan ProcessUpdateInfo -> IO ()
sigchldHandler stmChan =
    do
        result <- try $ getAnyProcessStatus False True :: IO (Either IOError (Maybe (ProcessID, ProcessStatus)))
        case result of 
            Left _ -> do
                errno <- getErrno
                when (errno /= eCHILD) $
                    -- If errno == ECHILD then our process doesn't have any children, so we simply end the computation
                    throwErrno "Error after getAnyProcessStatus in sigchldHandler."
            Right maybeProcState ->
                case maybeProcState of
                    Nothing -> return ()
                    Just (pid, procState) -> do
                        debug $ "In sigchldHandler. Received " ++ show (pid, procState)
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

    -- Craft initial state of shell
    let state = JobsState {
        jobs = IntMap.empty,
        terminalFd = ttyFd,
        stmChannel = stmChan
    }

    -- Take control of the terminal.
    pgid <- getProcessGroupID
    setTerminalPgid state pgid

    -- Return initial state of shell
    return state

-- Uses sigSuspend to wait for a sigChld.
-- Then it updates the current state based on the information from sigchldHandler()
waitForSigchld :: SignalSet -> StateT JobsState IO ()
waitForSigchld signalSet =
    do
        liftIO $ awaitSignal $ Just signalSet -- Wait for a sigChld and install old mask (where sigChld is unblocked)
        liftIO yield -- Give a chance for the sighchldHandler to run

        liftIO $ debug "In waitForSigchld received signal"

        initialState <- get
        let stmChan = stmChannel initialState
        stateAfterChannelRead <- liftIO $ atomically $ readChannelAndUpdateState stmChan initialState

        liftIO $ debug $ "In waitForSigchld read updated state from channel. state=" ++ show stateAfterChannelRead

        liftIO $ sendPendingSignals $ jobs stateAfterChannelRead
        let finalState = removePendingSignalsFromState stateAfterChannelRead

        liftIO $ debug $ "In waitForSigchld after sending pending signals. state=" ++ show finalState

        put finalState

    where
        -- This is called after awaitSignal returned, so there is at least one child process which changed state.
        -- It waits for that one ProcessUpdateInfo in a TChan in a blocking way (because sigChldHandler might not have yet sent update info to TChan).
        -- After this it reads all remaining ProcessUpdateInfo's in a non blocking way (because there might be many process which changed state, we don't know how many) 
        readChannelAndUpdateState :: TChan ProcessUpdateInfo -> JobsState -> STM JobsState
        readChannelAndUpdateState chan initialState = do
            stateAfterFirstUpdate <- readChannelItemBlockingAndUpdateState chan initialState
            readChannelNonBlockingAndUpdateState chan stateAfterFirstUpdate

        readChannelItemBlockingAndUpdateState :: TChan ProcessUpdateInfo -> JobsState -> STM JobsState
        readChannelItemBlockingAndUpdateState chan initialState = do
            updateInfo <- readTChan chan
            return $ updateState initialState updateInfo

        readChannelNonBlockingAndUpdateState :: TChan ProcessUpdateInfo -> JobsState -> STM JobsState
        readChannelNonBlockingAndUpdateState chan initialState = do
            maybeUpdateInfo <- tryReadTChan chan
            case maybeUpdateInfo of 
                Nothing -> return initialState
                Just updateInfo ->
                    readChannelNonBlockingAndUpdateState chan $ updateState initialState updateInfo

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
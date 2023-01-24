module Jobs(initJobs) where

import JobsState (JobsState (..), Job (pendingSignalsForProcessGroup, pgid), ProcessUpdateInfo, initialJobsState, updateState)


import Control.Concurrent.STM
    ( STM, atomically, newTChan, tryReadTChan, writeTChan, TChan )
import qualified Data.IntMap as IntMap
import System.Posix
import ProcState ( processStatusToProcState )
import Control.Monad.IO.Class ( MonadIO(liftIO) ) 
import Control.Monad ( guard )
import Control.Concurrent ( yield )
import Control.Monad.Trans.State (StateT, put, get)

sigchldMask :: SignalSet
sigchldMask = addSignal processStatusChanged emptySignalSet

-- TODO:
-- resumeJob - after sending SIGCONT update job state to RUNNING. 
--             Can't do that from sigchildHandler, because unix Haskell library doesn't have bindings
--             for WCONTINUED flag in waitpid() :/
-- killJob
-- watchJobs 
-- monitorJob
-- shutdownJobs

-- monitorJob :: SignalSet -> StateT JobsState IO ()
-- monitorJob signalSet =
--     do
--         state <- get
--         let fgJob = jobs state IntMap.! fgIdx in
--         setTerminalProcessGroupID 


sigchldHandler :: TChan ProcessUpdateInfo -> IO ()
sigchldHandler stmChan =
    do
        maybeProcState <- getAnyProcessStatus False True 
        case maybeProcState of
            Nothing -> return ()
            Just (pid, procState) -> do
                atomically $ writeTChan stmChan (pid, processStatusToProcState procState)
                sigchldHandler stmChan


initJobs :: StateT JobsState IO ()
initJobs = do
    stmChan <- liftIO $ atomically newTChan

    --    Block SIGINT for the duration of `sigchldHandler`
    --    in case `sigintHandler` does something crazy like `longjmp`.
    let signalMaskToBlock = addSignal keyboardSignal emptySignalSet
    _ <- liftIO $ installHandler processStatusChanged (Catch $ sigchldHandler stmChan) (Just signalMaskToBlock)

    -- Assume we're running in interactive mode, so move us to foreground.
    -- Duplicate terminal fd, but do not leak it to subprocesses that execve.
    isTerminal <- liftIO $ queryTerminal stdInput
    guard isTerminal 
    ttyFd <- liftIO $ dup stdInput
    liftIO $ setFdOption ttyFd CloseOnExec True

    put initialJobsState {
        jobs = IntMap.empty,
        terminalFd = ttyFd,
        stmChannel = stmChan
    }

    -- Take control of the terminal.
    pgid <- liftIO getProcessGroupID
    liftIO $ setTerminalProcessGroupID ttyFd pgid

-- Uses sigSuspend to wait for a sigChld.
-- Then it updates the current state based on the information from sigchldHandler()
waitForSigchld :: StateT JobsState IO ()
waitForSigchld =
    do
        liftIO $ awaitSignal $ Just sigchldMask -- Wait for a sigChld 
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
module StmChannelCommunication (
    updateStateFromChannelBlocking,
    updateStateFromChannelNonBlocking,
    updateStateFromChannel,
    readChannelItemBlockingUntilJobIdFoundAndUpdateState,
    readChannelNonBlockingAndUpdateState) where

import JobsState (JobID, JobsState (stmChannel, jobs), ProcessUpdateInfo, Job (processes, pgid, pendingSignalsForProcessGroup), removePendingSignalsFromState, Process (pid), updateState)
import Control.Monad.Trans.State (StateT, get, put)
import Control.Concurrent.STM (STM, TChan, atomically, readTChan, tryReadTChan)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import DebugLogger (debug)
import System.Posix (signalProcessGroup)


updateStateFromChannelBlocking :: JobID -> StateT JobsState IO ()
updateStateFromChannelBlocking expectedJobId = do
    initialState <- get
    let stmChan = stmChannel initialState

    updateStateFromChannel $ 
            readChannelItemBlockingUntilJobIdFoundAndUpdateState expectedJobId stmChan initialState

updateStateFromChannelNonBlocking :: StateT JobsState IO ()
updateStateFromChannelNonBlocking = do
    initialState <- get
    let stmChan = stmChannel initialState

    updateStateFromChannel $ readChannelNonBlockingAndUpdateState stmChan initialState

updateStateFromChannel :: STM JobsState -> StateT JobsState IO ()
updateStateFromChannel stmComputation = do
    stateAfterChannelRead <- liftIO $ atomically stmComputation
    liftIO $ debug $ "In updateStateFromChannel read updated state from channel. state=" ++ show stateAfterChannelRead

    liftIO $ sendPendingSignals $ jobs stateAfterChannelRead
    let finalState = removePendingSignalsFromState stateAfterChannelRead
    liftIO $ debug $ "In updateStateFromChannel after sending pending signals. state=" ++ show finalState

    put finalState


-- Read blocking until job with id == expectedJobId appears. Then read the rest non-blocking
readChannelItemBlockingUntilJobIdFoundAndUpdateState :: JobID -> TChan ProcessUpdateInfo -> JobsState -> STM JobsState
readChannelItemBlockingUntilJobIdFoundAndUpdateState expectedJobId chan initialState = do
    (processId, procState) <- readTChan chan
    let expectedJob = jobs initialState IntMap.! expectedJobId
    let maybeProcInExpectedJob = List.find (\process -> pid process == processId) $ processes expectedJob
    let newState = updateState initialState (processId, procState) 
    
    case maybeProcInExpectedJob of
        Nothing ->
            -- Process which updated its state is NOT the one we have been looking for.
            -- Try to read the channel blocking again
            readChannelItemBlockingUntilJobIdFoundAndUpdateState expectedJobId chan newState
        Just _ ->
            -- Process which updated its state IS the one we have been looking for.
            -- Read the rest of the updates from channel non blocking.
            readChannelNonBlockingAndUpdateState chan newState


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
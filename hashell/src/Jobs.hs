module Jobs (JobsState, Job, Process, ProcState,
 jobExitCode, addJob, delJob, moveFGJobToBG, moveBGJobToFG) where

import Control.Exception (assert)
import Control.Monad.Trans.State.Lazy ( StateT, put, get )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Posix (ProcessID, ProcessGroupID, queryTerminal, stdInput, Fd, dup, setFdOption, FdOption (CloseOnExec), setTerminalProcessGroupID, getProcessGroupID, TerminalMode, ProcessStatus, getAnyProcessStatus, Handler (Catch), awaitSignal, signalProcessGroup)
import System.Posix.Signals
    ( addSignal,
      emptySignalSet,
      installHandler,
      keyboardSignal,
      processStatusChanged,
      Handler(CatchInfo),
      SignalInfo, SignalSet, Signal )
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap
import System.Console.Isocline (termWriteLn)
import Control.Monad (guard)
import Control.Concurrent.STM
    ( atomically, newTChan, writeTChan, TChan, tryReadTChan, STM )
import Control.Concurrent (yield)


------------------------ Useful constants ------------------------------

fgIdx :: IntMap.Key
fgIdx = 0

bgIdx :: IntMap.Key
bgIdx = 1

sigchldMask :: SignalSet
sigchldMask = addSignal processStatusChanged emptySignalSet


--------------------------- Types ------------------------------

type JobID = Int
type ExitCode = Int

type ProcessUpdateInfo = (ProcessID, ProcessStatus)

data JobsState = JobsState {
    -- Job with index 0 is the foreground job
    jobs :: IntMap.IntMap Job,
    terminalFd :: Fd,
    stmChannel :: TChan ProcessUpdateInfo
}

data Job = Job {
    pgid :: ProcessGroupID,
    processes :: [Process],
    jobState :: ProcState,
    cmdString :: String,

    -- These signals will be sent to this job's process group soon.
    -- It's here, because if one of the processes got a SIGTSTP or SIGCONT,
    -- we want to send the same signal to other processes in this job.
    pendingSignalsForProcessGroup :: [Signal]  
} deriving Show

data Process = Process {
    pid :: ProcessID,
    procState :: ProcState,
    exitCode :: ExitCode
} deriving Show

data ProcState = ALL | RUNNING | STOPPED | FINISHED deriving (Show, Eq)



----------------------------- Public functions ---------------------------

-- When pipeline is done, its exitcode is fetched from the last process.
jobExitCode :: Job -> ExitCode
jobExitCode job = exitCode $ List.last $ processes job


addJob :: JobsState -> ProcessGroupID -> Bool -> String -> (JobsState, JobID)
addJob state pgid isBackground cmdString =
    let newJob = Job {
        pgid = pgid,
        processes = [],
        jobState = RUNNING,
        cmdString = cmdString
    } in
    insertJob state newJob isBackground


delJob :: JobsState -> JobID -> JobsState
delJob state idx =
    let allJobs = jobs state in
    let job = allJobs IntMap.! idx in
        assert (jobState job == FINISHED) $
        state {jobs = IntMap.delete idx allJobs}


moveFGJobToBG :: JobsState -> JobsState
moveFGJobToBG state = 
    let allJobs = jobs state in
    let fgJob = allJobs IntMap.! fgIdx in
    let stateWithoutFGJob = state {jobs = IntMap.delete fgIdx allJobs} in
    fst $ insertJob stateWithoutFGJob fgJob True


moveBGJobToFG :: JobsState -> JobID -> JobsState
moveBGJobToFG state idx =
    let allJobs = jobs state in
    let bgJob = allJobs IntMap.! idx in
    let stateWithoutBGJob = state {jobs = IntMap.delete idx allJobs} in
    fst $ insertJob stateWithoutBGJob bgJob False
        

addProc :: JobsState -> JobID -> ProcessID -> JobsState
addProc state idx pid =
    let newProcess = Process {
        pid = pid,
        procState = RUNNING,
        exitCode = -1
    } in
    let job = jobs state IntMap.! idx in
    let newJob = job {processes = processes job ++ [newProcess]} in
    state {jobs = IntMap.insert idx newJob $ jobs state}


getJobState :: JobsState -> JobID -> ProcState
getJobState state idx = jobState $ jobs state IntMap.! idx 


-- Removes the finished job and returns its exit code
cleanUpFinishedJob :: JobsState -> JobID -> (JobsState, ExitCode)
cleanUpFinishedJob state idx =
    let exCode = jobExitCode (jobs state IntMap.! idx) in
    (delJob state idx, exCode)

-- monitorJob :: SignalSet -> StateT JobsState IO ()
-- monitorJob signalSet =
--     do
--         state <- get
--         let fgJob = jobs state IntMap.! fgIdx in
--         setTerminalProcessGroupID 


-- Uses sigSuspend to wait for a sigChld.
-- Then it updates the current state based on the information from sigchldHandler()
waitForSigchld :: StateT JobsState IO ()
waitForSigchld =
    do
        liftIO $ awaitSignal $ Just sigchldMask -- Wait for a sigChld 
        liftIO yield -- Give a chance for the sighchldHandler to run

        state <- get
        let stmChan = stmChannel state
        stateAfterChannelRead <- liftIO $ atomically $ readChannelAndUpdateState stmChan state
        liftIO $ sendPendingSignals $ jobs stateAfterChannelRead
        let finalState = removePendingSignalsFromState stateAfterChannelRead
        put finalState

    where
        readChannelAndUpdateState :: TChan ProcessUpdateInfo -> JobsState -> STM JobsState
        readChannelAndUpdateState chan state = do
            maybeUpdateInfo <- tryReadTChan chan
            case maybeUpdateInfo of 
                Nothing -> return state
                Just updateInfo ->
                    readChannelAndUpdateState chan $ updateState state updateInfo

        sendPendingSignals :: IntMap.IntMap Job -> IO ()
        sendPendingSignals =
            mapM_ 
            (\job -> 
                mapM_ 
                (\sig -> signalProcessGroup sig $ pgid job) 
                $ pendingSignalsForProcessGroup job)

        removePendingSignalsFromState :: JobsState -> JobsState
        removePendingSignalsFromState state =
            state {
                jobs = 
                    IntMap.map 
                    (\job -> job {pendingSignalsForProcessGroup = []}) 
                    $ jobs state
            }



updateState :: JobsState -> ProcessUpdateInfo -> JobsState
updateState state processUpdateInfo = state


        

sigchldHandler :: TChan ProcessUpdateInfo -> IO ()
sigchldHandler stmChan =
    do
        maybeProcState <- getAnyProcessStatus False True 
        case maybeProcState of
            Nothing -> return ()
            Just (pid, procState) -> do
                atomically $ writeTChan stmChan (pid, procState)
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

    put JobsState {
        jobs = IntMap.empty,
        terminalFd = ttyFd,
        stmChannel = stmChan
    }

    -- Take control of the terminal.
    pgid <- liftIO getProcessGroupID
    liftIO $ setTerminalProcessGroupID ttyFd pgid



-- TODO:
-- resumeJob - after sending SIGCONT update job state to RUNNING. 
--             Can't do that from sigchildHandler, because unix Haskell library doesn't have bindings
--             for WCONTINUED flag in waitpid() :/
-- killJob
-- watchJobs 
-- monitorJob
-- shutdownJobs



-------------------------------- Private functions ------------------------------

insertJob :: JobsState -> Job -> Bool -> (JobsState, JobID)
insertJob state job isBackground =
    if isBackground then
        let jobIdx = findFirstEmptySlotForBackgroundJob state in
        (state {jobs = IntMap.insert jobIdx job $ jobs state}, jobIdx)
    else 
        (state {jobs = IntMap.insert fgIdx job $ jobs state}, fgIdx)


findFirstEmptySlotForBackgroundJob :: JobsState -> JobID
findFirstEmptySlotForBackgroundJob state =
    let allJobs = jobs state in   
    if IntMap.null allJobs
        then bgIdx
    else findFirstEmptyRec bgIdx allJobs

    where
    findFirstEmptyRec idx jobs = 
        if IntMap.notMember idx jobs 
            then idx
        else findFirstEmptyRec (idx + 1) jobs
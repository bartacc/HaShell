module Jobs (JobsState, Job, Process, ProcState,
 initialJobsState, jobExitCode, addJob, delJob, moveFGJobToBG, moveBGJobToFG) where

import Control.Exception (assert)
import Control.Monad.Trans.State.Lazy ( StateT, put )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Posix (ProcessID, ProcessGroupID, queryTerminal, stdInput, Fd, dup, setFdOption, FdOption (CloseOnExec), setTerminalProcessGroupID, getProcessGroupID, TerminalMode)
import System.Posix.Signals
    ( addSignal,
      emptySignalSet,
      installHandler,
      keyboardSignal,
      processStatusChanged,
      Handler(CatchInfo),
      SignalInfo )
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap
import System.Console.Isocline (termWriteLn)
import Control.Monad (guard)
import Control.Concurrent


fgIdx = 0
bgIdx = 1

type JobID = Int
type ExitCode = Int

data JobsState = JobsState {
    -- Job with index 0 is the foreground job
    jobs :: IntMap.IntMap Job,
    terminalFd :: Fd
} deriving Show

data Job = Job {
    pgid :: ProcessGroupID,
    processes :: [Process],
    jobState :: ProcState,
    cmdString :: String
} deriving Show

data Process = Process {
    pid :: ProcessID,
    procState :: ProcState,
    exitCode :: ExitCode
} deriving Show

data ProcState = ALL | RUNNING | STOPPED | FINISHED deriving (Show, Eq)

initialJobsState :: JobsState
initialJobsState = JobsState {
    jobs = IntMap.empty,
    terminalFd = -1
}

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

sigchldHandler :: MVar Int -> SignalInfo -> IO ()
sigchldHandler mvar sigInfo =
    do
        termWriteLn "sigchldHandler"


initJobs :: StateT JobsState IO ()
initJobs = do
    myMVar <- liftIO newEmptyMVar

    --    Block SIGINT for the duration of `sigchldHandler`
    --    in case `sigintHandler` does something crazy like `longjmp`.
    let signalMaskToBlock = addSignal keyboardSignal emptySignalSet
    _ <- liftIO $ installHandler processStatusChanged (CatchInfo $ sigchldHandler myMVar) (Just signalMaskToBlock)

    -- Assume we're running in interactive mode, so move us to foreground.
    -- Duplicate terminal fd, but do not leak it to subprocesses that execve.
    isTerminal <- liftIO $ queryTerminal stdInput
    guard isTerminal 
    ttyFd <- liftIO $ dup stdInput
    liftIO $ setFdOption ttyFd CloseOnExec True

    put initialJobsState {terminalFd = ttyFd}

    -- Take control of the terminal.
    pgid <- liftIO getProcessGroupID
    liftIO $ setTerminalProcessGroupID ttyFd pgid



-- TODO:
-- sigchldHandler
-- resumeJob - after sending SIGCONT update job state to RUNNING. 
--             Can't do that from sigchildHandler, because unix Haskell library doesn't have bindings
--             for WCONTINUED flag in waitpid() :/
-- killJob
-- watchJobs 
-- monitorJob
-- shutdownJobs



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
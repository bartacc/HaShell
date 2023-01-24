module JobsState (JobsState(..), Job(..), Process, ProcState, ProcessUpdateInfo,
 addJob, delJob, moveFGJobToBG, moveBGJobToFG, updateState, initialJobsState, addProc, getJobState, cleanUpFinishedJob, getLastProcState) where

import ProcState
    ( ProcState(STOPPED, RUNNING),
      isFinished )

import Control.Exception (assert)
import System.Posix (ProcessID, ProcessGroupID, Fd)
import System.Posix.Signals
    ( Signal, sigCONT, sigTSTP )
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent.STM
    ( TChan )
import Data.Maybe (isJust)

------------------------ Useful constants ------------------------------

fgIdx :: IntMap.Key
fgIdx = 0

bgIdx :: IntMap.Key
bgIdx = 1

--------------------------- Types ------------------------------

type JobID = Int

type ProcessUpdateInfo = (ProcessID, ProcState)

data JobsState = JobsState {
    -- Job with index 0 is the foreground job
    jobs :: IntMap.IntMap Job,
    terminalFd :: Fd,
    stmChannel :: TChan ProcessUpdateInfo
}

instance Show JobsState where
  show s = show $ jobs s

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
    procState :: ProcState
} deriving Show

----------------------------- Public functions ---------------------------
initialJobsState :: JobsState 
initialJobsState = JobsState {
    jobs = IntMap.empty
}

addJob :: JobsState -> ProcessGroupID -> Bool -> String -> (JobsState, JobID)
addJob state pgid isBackground cmdString =
    let newJob = Job {
        pgid = pgid,
        processes = [],
        jobState = RUNNING,
        cmdString = cmdString,
        pendingSignalsForProcessGroup = []
    } in
    insertJob state newJob isBackground


delJob :: JobsState -> JobID -> JobsState
delJob state idx =
    let allJobs = jobs state in
    let job = allJobs IntMap.! idx in
        assert (isFinished $ jobState job) $
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
        procState = RUNNING
    } in
    let job = jobs state IntMap.! idx in
    let newJob = job {processes = processes job ++ [newProcess]} in
    state {jobs = IntMap.insert idx newJob $ jobs state}


getJobState :: JobsState -> JobID -> ProcState
getJobState state idx = jobState $ jobs state IntMap.! idx 

-- When pipeline is done, its exitcode is fetched from the last process.
getLastProcState :: JobsState -> JobID -> ProcState
getLastProcState state idx = procState $ List.last $ processes $ jobs state IntMap.! idx


-- Removes the finished job and returns its status
cleanUpFinishedJob :: JobsState -> JobID -> (JobsState, ProcState)
cleanUpFinishedJob state idx =
    let lastProcState = getLastProcState state idx in
    (delJob state idx, lastProcState)


updateState :: JobsState -> ProcessUpdateInfo -> JobsState
updateState state (procId, newProcState) = 
    let jobsWithSpecifiedProc = 
            IntMap.filter 
            (isJust . List.find (\proc -> pid proc == procId) . processes) 
            $ jobs state in
    assert (IntMap.size jobsWithSpecifiedProc == 1) $
    let (jobID, jobToUpdate) = head $ IntMap.assocs jobsWithSpecifiedProc in
    let newProcessesList = 
            List.map
            (\proc -> 
                if pid proc == procId || newProcState == RUNNING then
                    -- If newProcState is RUNNING, it means that this proc received SIGCONT.
                    -- We will soon send the same signal to other processes in job,
                    -- but sigchldHandler won't send us this information, because getAnyProcessStatus
                    -- doesn't inform us about SIGCONT (no WCONTINUED flag support in Haskell's binding of waitpid).
                    -- That's why we have to update it's state here.
                    proc {procState = newProcState}
                else 
                    proc)
            $ processes jobToUpdate 
    in
    let newJobState = getNewJobState (jobState jobToUpdate) newProcessesList in
    let newJob = jobToUpdate {
       processes = newProcessesList, 
       jobState = newJobState,
       pendingSignalsForProcessGroup = 
           getSignalsToSend 
           (List.length newProcessesList) 
           (jobState jobToUpdate)
           (pendingSignalsForProcessGroup jobToUpdate) 
    } in
    state {jobs = IntMap.insert jobID newJob $ jobs state} 

    where 
        -- Updates the Job State if all processes in job have the same proc state
        getNewJobState :: ProcState -> [Process] -> ProcState 
        getNewJobState currentJobState procs =
            let allProcsInJobHaveFinished =
                    List.all 
                    (isFinished . procState)
                    procs
            in
            if allProcsInJobHaveFinished || not (isFinished newProcState) then
                newProcState
            else 
                currentJobState
        
        getSignalsToSend :: Int -> ProcState -> [Signal] -> [Signal]
        getSignalsToSend numOfProcsInJob oldJobState curPendingSignals =
            if numOfProcsInJob > 1 && oldJobState /= newProcState then
                case newProcState of 
                    RUNNING -> curPendingSignals ++ [sigCONT]
                    STOPPED _ -> curPendingSignals ++ [sigTSTP]
                    _ -> curPendingSignals
            else
                curPendingSignals


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
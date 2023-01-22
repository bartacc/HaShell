module Jobs (JobsState, Job, Process, ProcState, initialJobsState) where

import System.Posix (ProcessID, ProcessGroupID)

data JobsState = JobsState {
    jobs :: [Job]
}

data Job = Job {
    pgid :: ProcessGroupID,
    processes :: [Process],
    jobState :: ProcState
}

data Process = Process {
    pid :: ProcessID,
    procState :: ProcState,
    exitCode :: Int
}

data ProcState = RUNNING | STOPPED | FINISHED

initialJobsState :: JobsState
initialJobsState = JobsState {jobs = []}
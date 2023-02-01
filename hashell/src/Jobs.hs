module Jobs(initJobs, monitorJob, watchJobs, killJob, resumeJob, shutdownJobs, sigchldMask, setTerminalPgid) where

import JobsState (JobsState (..), Job (pgid, jobState, cmdString, processes), Process(..), ProcessUpdateInfo, getFgJob, getFgJobState, moveFGJobToBG, delJob, JobID, fgIdx, moveBGJobToFG)
import qualified UserMessages


import Control.Concurrent.STM
    ( atomically, newTChan, writeTChan, TChan )
import qualified Data.IntMap as IntMap
import System.Posix
import ProcState ( processStatusToProcState, ProcState (..), isFinished )
import Control.Monad.IO.Class ( MonadIO(liftIO) ) 
import Control.Monad ( guard, when )
import Control.Concurrent ( yield )
import Control.Monad.Trans.State (StateT, put, get)
import DebugLogger (debug)
import Control.Exception (try)
import Foreign.C (getErrno, eCHILD, throwErrno)
import UserMessages (getMessageBasedOnState, printMessage, printContinued, printMessageLn)
import StmChannelCommunication
    ( updateStateFromChannelBlocking,
      updateStateFromChannelNonBlocking )

sigchldMask :: SignalSet
sigchldMask = addSignal processStatusChanged emptySignalSet

shutdownJobs :: StateT JobsState IO ()
shutdownJobs = do
    oldMask <- liftIO getSignalMask
    liftIO $ blockSignals sigchldMask

    state <- get
    let allJobIds = IntMap.keys $ jobs state
    -- Kill remaining jobs and wait for them to finish.
    mapM_
        (\jId -> killJob jId oldMask)
        allJobIds

    watchJobs True
    liftIO $ setSignalMask oldMask
    liftIO $ closeFd $ terminalFd state


resumeJob :: Bool -> JobID -> SignalSet -> StateT JobsState IO ()
resumeJob isBg jId sigMask = do
    state <- get
    let jobMap = jobs state

    if IntMap.null jobMap then do
        liftIO $ printMessageLn "There are no jobs in the background"
        return ()
    else do
        let jobId = 
                if jId < 0 then fst $ IntMap.findMax jobMap
                else jId
        let job = jobMap IntMap.! jobId

        liftIO $ signalProcessGroup sigCONT $ pgid job
        liftIO $ printContinued jobId (cmdString job)

        -- After sending SIGCONT, update job state to RUNNING, by sending messages through STM TChan.
        -- Can't do that from sigchildHandler, because unix Haskell library doesn't have bindings
        -- for WCONTINUED flag in waitpid() :/
        let stmChan = stmChannel state
        liftIO $ atomically $
            mapM_
            (writeTChan stmChan)
            (craftUpdateProcInfo job)

        if isBg then
            -- Makes sure that shell received SIGCHLD and that job status is updated
            waitForSigchld sigMask jobId 
        else do
            let newState = moveBGJobToFG state jobId
            put newState
            monitorJob sigMask

    where
        craftUpdateProcInfo :: Job -> [ProcessUpdateInfo] 
        craftUpdateProcInfo job = 
            map 
            (\process -> (pid process, RUNNING)) 
            (processes job)



killJob :: JobID -> SignalSet -> StateT JobsState IO ()
killJob jobId _ = do
    state <- get
    let maybeJob = IntMap.lookup jobId $ jobs state
    case maybeJob of 
        Nothing -> liftIO $ printMessageLn $ "Job with id " ++ show jobId ++ " not found"
        Just job ->
            if isFinished $ jobState job then
                return ()
            else do
                let jobPgid = pgid job
                liftIO $ signalProcessGroup sigTERM jobPgid 
                liftIO $ signalProcessGroup sigCONT jobPgid

                updateStateFromChannelBlocking jobId

-- Read (non-blocking) updates from STM Channel, then print info about jobs state. Also delete finished jobs
watchJobs :: Bool -> StateT JobsState IO ()
watchJobs watchOnlyFinished =
    do
        updateStateFromChannelNonBlocking

        state <- get
        let (finalState, message) = 
                IntMap.foldlWithKey
                (\ (curState, curMessage) jobID job -> 
                    let thisJobState = jobState job in
                    if jobID == fgIdx || (watchOnlyFinished && not (isFinished thisJobState)) then
                        (curState, curMessage)
                    else
                        let msg = curMessage ++ getMessageBasedOnState jobID (cmdString job) thisJobState in
                        if isFinished thisJobState then
                            (delJob curState jobID, msg)
                        else
                            (curState, msg))
                (state, "")
                (jobs state)
        liftIO $ printMessage message
        put finalState


setTerminalPgid :: JobsState -> ProcessGroupID -> IO ()
setTerminalPgid state newControllingPgid =
    do
        let ttyFd = terminalFd state
        isValidTerminalFd <- queryTerminal ttyFd
        guard isValidTerminalFd
        setTerminalProcessGroupID ttyFd newControllingPgid
        debug $ "After setTerminalProcessGroupID ttyFd=" ++ show (terminalFd state) ++ " pgid=" ++ show newControllingPgid ++ " isValidTtyFd=" ++ show isValidTerminalFd

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
                waitForSigchld signalSet fgIdx
                liftIO $ debug $ "monitorJob after waitForSigchld"

                state <- get 
                let fgJobState = getFgJobState state in
                    case fgJobState of
                        STOPPED _ -> do
                            let (newState, bgJobId) = moveFGJobToBG state
                            put newState
                            liftIO $ UserMessages.printSuspended bgJobId $ cmdString $ jobs newState IntMap.! bgJobId
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
    shellPgid <- getProcessGroupID
    setTerminalPgid state shellPgid

    -- Return initial state of shell
    return state

-- Uses sigSuspend to wait for a sigChld.
-- Then it updates the current state based on the information from sigchldHandler()
waitForSigchld :: SignalSet -> JobID -> StateT JobsState IO ()
waitForSigchld signalSet jobIdToWaitFor =
    do
        liftIO $ awaitSignal $ Just signalSet -- Wait for a sigChld and install old mask (where sigChld is unblocked)
        liftIO yield -- Give a chance for the sighchldHandler to run

        liftIO $ debug "In waitForSigchld received signal"

        updateStateFromChannelBlocking jobIdToWaitFor
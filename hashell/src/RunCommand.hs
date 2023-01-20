module RunCommand where

import Parser
import Jobs

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.Posix (Fd, ProcessID)
import System.Console.Isocline (termWriteLn)

data ProcessToRun = ProcessToRun {
    cmdName :: String,
    args :: [String],
    inputFD :: Fd,
    outputFD :: Fd,
    isBackground :: Bool
}


run :: Command -> StateT JobsState IO ()
run (SingleCommand cmd isBackground) =
        do 
                liftIO $ putStr "SingleCommand Done"

run (PipelineCommand cmds isBackground) =
        do
                liftIO $ putStr "PipelineCommand Done"


startProcess :: ProcessToRun -> IO ()
startProcess (ProcessToRun cmdName args inputFD outputFD isBackground) =
        termWriteLn cmdName
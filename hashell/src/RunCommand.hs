module RunCommand where

import Parser
import JobsState

import Control.Monad.Trans.State.Lazy ( StateT )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Posix (Fd)
import System.Console.Isocline (termWriteLn)

data ProcessToRun = ProcessToRun {
    cmdName :: String,
    args :: [String],
    inputFD :: Fd,
    outputFD :: Fd,
    isBackground :: Bool
}


run :: CommandToRun -> StateT JobsState IO ()
run (CommandToRun (SingleCommand cmd) cmdString isBackground) =
        do 
                liftIO $ putStr "Command Done"

run (CommandToRun (PipelineCommand cmds) cmdString isBackground) =
        do 
                liftIO $ putStr "Command Done"


startProcess :: ProcessToRun -> IO ()
startProcess (ProcessToRun cmdName args inputFD outputFD isBackground) =
        termWriteLn cmdName
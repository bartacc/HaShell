module BuiltinCommand(isBuiltinCmd, runBuiltinCmd) where
    
import ProcessToRun (ProcessToRun)
import JobsState (JobsState)
import Control.Monad.Trans.State (StateT, get, put)

isBuiltinCmd :: ProcessToRun -> Bool
isBuiltinCmd procToRun = False

runBuiltinCmd :: ProcessToRun -> StateT JobsState IO ()
runBuiltinCmd procToRun = 
    do
        state <- get
        put state



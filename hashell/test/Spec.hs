main :: IO ()
main = putStrLn "Test suite not yet implemented"

{-

import System.Exit
import Jobs

let (s1, _) = addJob initialJobsState 100 False "cmd1"
let (s2, _) = addJob s1 200 True "cmd2"
let (s3, _) = addJob s2 200 True "cmd3"
let s4 = addProc s3 0 101
let s5 = addProc s4 0 102
let s6 = addProc s5 1 201
let s7 = addProc s6 1 202
updateState (updateState s7 (102, TERMINATED sigKILL)) (101, EXITED ExitSuccess)
updateState (updateState s7 (201, STOPPED sigTSTP)) (202, STOPPED sigTSTP)
let s8 = updateState (updateState s7 (201, STOPPED sigTSTP)) (202, STOPPED sigTSTP)

-}


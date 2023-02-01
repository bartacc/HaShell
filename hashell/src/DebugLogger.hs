module DebugLogger (debug) where
import System.Console.Isocline (termWriteLn)
import Control.Monad (when)

debugOn :: Bool
debugOn = True

debug :: String -> IO ()
debug content = when debugOn $ termWriteLn content
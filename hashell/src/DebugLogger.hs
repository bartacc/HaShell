module DebugLogger (debug) where
import System.Console.Isocline (termWriteLn)
import Control.Monad (when)

debugOn :: Bool
debugOn = False

debug :: String -> IO ()
debug content = when debugOn $ termWriteLn content
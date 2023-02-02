module Main (main) where
import Control.Monad
import System.Posix
import System.Console.Isocline
import Jobs (initJobs, watchJobs)
import JobsState (JobsState)
import Parser (parse)
import Control.Monad.Trans.State (execStateT)
import RunCommand (run)
import DebugLogger (debug)
import Data.Char (isSpace)
import BuiltinCommand (doQuit)

main :: IO ()
main = do
--  is terminal in interactive mode?
  isAtty <- queryTerminal 0
  guard isAtty

  pid <- getProcessID
  _ <- createProcessGroupFor pid

  initialState <- initJobs

  debug $ show initialState

    -- TODO: 
    --  struct sigaction act = {
    --    .sa_handler = sigint_handler,
    --    .sa_flags = 0, /* without SA_RESTART read() will return EINTR */
    --  };
    --  Sigaction(SIGINT, &act, NULL);

    -- Shell should ignore SIGINT, SIGTSTP, SIGTTIN, SIGTTOU signals
  _ <- installHandler keyboardSignal Ignore Nothing
  _ <- installHandler keyboardStop Ignore Nothing
  _ <- installHandler backgroundRead Ignore Nothing
  _ <- installHandler backgroundWrite Ignore Nothing


  setHistory "history.txt" 200
  readPrompt initialState


readPrompt :: JobsState -> IO ()
readPrompt state = do 
  maybeLine <- readlineMaybe "$"
  case maybeLine of 
    Nothing -> do
      _ <- execStateT (doQuit []) state
      return ()
    Just line -> do

      if not $ all isSpace line then do
        historyAdd line

        let parsedCmd = parse line
        debug $ show parsedCmd
        
        stateAfterEval <- execStateT (run parsedCmd) state
        stateAfterWatchJobs <- execStateT (watchJobs True) stateAfterEval

        readPrompt stateAfterWatchJobs
      else 
        readPrompt state

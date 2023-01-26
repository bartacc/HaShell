module Main (main) where
import Control.Monad
import Data.Maybe
import System.Posix
import System.Console.Isocline
import Jobs (initJobs)
import JobsState (JobsState)
import Parser (parse)
import Control.Monad.Trans.State (execStateT)
import RunCommand (run)

main :: IO ()
main = do
--  is terminal in interactive mode?
  isAtty <- queryTerminal 0
  guard isAtty

  pid <- getProcessID
  _ <- createProcessGroupFor pid

  initialState <- initJobs

  termWriteLn $ show initialState

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

  termWriteLn "Done"


readPrompt :: JobsState -> IO ()
readPrompt state = do 
  maybeLine <- readlineMaybe "$"
  case maybeLine of 
    Nothing -> return () -- TODO: shutdownJobs()
    Just line -> do
      historyAdd line
      let parsedCmd = parse line
      termWriteLn $ show parsedCmd
      newState <- execStateT (run parsedCmd) state
      -- TODO: watchjobs(FINISHED);
      readPrompt newState

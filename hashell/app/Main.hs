module Main (main) where
import Control.Monad
import Data.Maybe
import System.Posix
import System.Console.Isocline

main :: IO ()
main = do
--  is terminal in interactive mode?
  isAtty <- queryTerminal 0
  guard isAtty

  _ <- createProcessGroupFor 0

    -- TODO: initjobs()

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
  readPrompt

  termWriteLn "Done"


readPrompt :: IO ()
readPrompt = do 
  maybeLine <- readlineMaybe "$"
  case maybeLine of 
    Nothing -> return () -- TODO: shutdownJobs()
    Just line -> do
      historyAdd line
      termWriteLn line -- TODO: eval line
      -- TODO: watchjobs(FINISHED);
      readPrompt

module Parser (CommandWithArgs(..), Command(..)) where

data CommandWithArgs = CommandWithArgs {
    cmdName :: String,
    args :: [String],
    inputFilePath :: Maybe String,
    outputFilePath :: Maybe String
} deriving Show

data Command = 
    SingleCommand {
        cmd :: CommandWithArgs,
        isBackground :: Bool
    } 
    | PipelineCommand {
        cmds :: [CommandWithArgs],
        isBackground :: Bool
    }
    deriving Show

parse :: String -> Command
parse text = 
    -- TODO
    SingleCommand {
        cmd = CommandWithArgs "Test" [] Nothing Nothing,
        isBackground = False
    }
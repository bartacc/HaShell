module Parser (CommandWithArgs(..), Command(..), CommandToRun(..), parse) where

import Data.List ( elemIndex )
import qualified Data.Text as T

data CommandWithArgs = CommandWithArgs {
    cmdName :: String,
    args :: [String],
    inputFilePath :: Maybe String,
    outputFilePath :: Maybe String
} deriving Show

data Command = 
      SingleCommand CommandWithArgs
    | PipelineCommand [CommandWithArgs]
    deriving Show

data CommandToRun = CommandToRun {
    cmd :: Command,
    isBackground :: Bool
} deriving Show


parse :: String -> CommandToRun
parse input = 
    let text = T.strip . T.pack $ input in
    case T.stripSuffix (T.singleton '&') text of
        Nothing -> 
            CommandToRun {
                cmd = parseSingleOrPipelineCommand text,
                isBackground = False
            }
        Just prefix -> 
            CommandToRun {
                cmd = parseSingleOrPipelineCommand prefix,
                isBackground = True
            }
    

parseSingleOrPipelineCommand :: T.Text -> Command
parseSingleOrPipelineCommand text =
    let pipeSymbol = T.singleton '|' in
    if T.isInfixOf pipeSymbol text then
        parsePipelineCommand $ T.splitOn pipeSymbol text
    else
        parseSingleCommand text


parseSingleCommand :: T.Text -> Command
parseSingleCommand text = SingleCommand $ parseCommandWithArgs text

parsePipelineCommand :: [T.Text] -> Command
parsePipelineCommand texts = PipelineCommand $ map parseCommandWithArgs texts

parseCommandWithArgs :: T.Text -> CommandWithArgs
parseCommandWithArgs text = 
    let (maybeInputPath, maybeOutputPath, remainingText) = getInputOutputPaths text in
    let words = map T.unpack $ T.words remainingText in
    if null words then
        error "Invalid command structure"
    else
        CommandWithArgs {
            cmdName = head words,
            args = tail words,
            inputFilePath = maybeInputPath,
            outputFilePath = maybeOutputPath   
        }


-- For a given command text return
-- Input file path (or Nothing)
-- Output file path (or Nothing)
-- Command text without redirections
getInputOutputPaths :: T.Text -> (Maybe String, Maybe String, T.Text)
getInputOutputPaths text = 
    let wordsReversed = reverse $ T.words text in
    let maybeInputIdx = elemIndex (T.singleton '<') wordsReversed in
    let maybeOutputIdx = elemIndex (T.singleton '>') wordsReversed in
    case maybeInputIdx of 
        Nothing -> 
            let (outputPath, wordsAfterOutput) = getPath wordsReversed (T.singleton '>') in
            (Nothing, outputPath, T.unwords $ reverse wordsAfterOutput)
        Just inputIdx -> 
            case maybeOutputIdx of
                Nothing -> 
                    let (inputPath, wordsAfterInput) = getPath wordsReversed (T.singleton '<') in
                    (inputPath, Nothing, T.unwords $ reverse wordsAfterInput)
                Just outputIdx ->
                    if inputIdx < outputIdx then
                        let (inputPath, wordsAfterInput) = getPath wordsReversed (T.singleton '<') in
                        let (outputPath, wordsAfterOutput) = getPath wordsAfterInput (T.singleton '>') in
                        (inputPath, outputPath, T.unwords $ reverse wordsAfterOutput)
                    else 
                        let (outputPath, wordsAfterOutput) = getPath wordsReversed (T.singleton '>') in
                        let (inputPath, wordsAfterInput) = getPath wordsAfterOutput (T.singleton '<') in
                        (inputPath, outputPath, T.unwords $ reverse wordsAfterInput)



-- For a reversed list of words in a command and a symbol ('>' or '<'),
-- return the path corresponding to this symbol (or Nothing) and the remaining words in a command 
getPath :: [T.Text] -> T.Text -> (Maybe String, [T.Text])
getPath wordsReversed symbol = 
    let maybeIdx = elemIndex symbol wordsReversed in
    case maybeIdx of
        Nothing -> (Nothing, wordsReversed)
        Just idx -> 
            if idx > 1 then 
                error $ "Path for " ++ T.unpack symbol ++ " redirection invalid"
            else
                (Just $ T.unpack $ head wordsReversed, drop 2 wordsReversed)
module ProcessToRun(ProcessToRun(..), createProcessToRun) where
    
import System.Posix (Fd, OpenMode (..), OpenFileFlags (..), openFd, stdFileMode)
import Parser (IsBackground, CommandWithArgs (..), cmdName, args)

data ProcessToRun = ProcessToRun {
    cmdName :: String,
    args :: [String],
    inputFD :: Fd,  -- By default -1
    outputFD :: Fd, -- By default -1 
    isBackground :: IsBackground
} deriving Show



createProcessToRun :: CommandWithArgs -> IsBackground -> IO ProcessToRun
createProcessToRun cmdWithArgs isBg =
        do
                inFd <- do 
                        case inputFilePath cmdWithArgs of
                                Nothing -> return (-1)
                                Just filePath -> getFdForFile filePath ReadOnly
                outFd <- do 
                        case outputFilePath cmdWithArgs of
                                Nothing -> return (-1)
                                Just filePath -> getFdForFile filePath WriteOnly

                return ProcessToRun {
                        ProcessToRun.cmdName = Parser.cmdName cmdWithArgs,
                        ProcessToRun.args = Parser.args cmdWithArgs,
                        inputFD = inFd,
                        outputFD = outFd,
                        ProcessToRun.isBackground = isBg
                }

getFdForFile :: FilePath -> OpenMode -> IO Fd
getFdForFile filePath mode = 
        openFd filePath mode OpenFileFlags {
                creat = Just stdFileMode, -- rw-rw-rw-
                append = False,
                exclusive = False,
                noctty = False,
                nonBlock = False,
                trunc = False,
                nofollow = False,
                cloexec = False,
                directory = False,
                sync = False
        } 
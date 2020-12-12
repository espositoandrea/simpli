module Cli (getInputProgram) where
import           Control.Monad
import           Data.Version          (showVersion)
import           Paths_simpli          (version)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

printVersion :: IO ()
printVersion = do
  putStrLn $ "SIMPLI (the Simple IMP Language Interpreter) " ++
    showVersion version ++ "\n" ++
    "Copyright (C) 2020, Andrea Esposito.\n" ++
    "Released under the GNU GPL v3 License <https://gnu.org/licenses/gpl-3.0.html>.\n" ++
    "This is free software: you are free to change and redistribute it.\n" ++
    "There is NO WARRANTY, to the extent permitted by law.\n" ++
    "\n" ++
    "Written by Andrea Esposito."

printHelp :: IO()
printHelp = do
  prg <- getProgName
  putStr $ "Usage: " ++ prg ++ " [options ...] [FILE | -c COMMAND]\n" ++
    "Launches the interpreter for the IMP Language using FILE as source.\n" ++
    "If FILE is not provided, the source code is read from stdin.\n" ++
    "\n" ++
    usageInfo "Available options:" options

data Options = Options { optCommand :: IO String }

startOptions :: Options
startOptions = Options { optCommand = return "" }

options :: [OptDescr (Options -> IO Options) ]
options =
  [ Option "v" ["version"]
      (NoArg (\_ -> do
        printVersion
        exitWith ExitSuccess))
     "Print version information and exit"
  , Option "h" ["help"]
      (NoArg (\_ -> do
        printHelp
        exitWith ExitSuccess))
      "Show this help message and exit"
  , Option "c" ["command"]
      (ReqArg (\arg opt -> return opt { optCommand = return arg }) "COMMAND")
      "A command to be executed instead of\nreading code from FILE"
  ]

-- |Get the source code that will be used as input by the interpreter
getInputProgram :: IO String
getInputProgram = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  when (not (null errors)) (do
    putStr "Error. "
    mapM putStr errors
    putStr "Use '"
    getProgName >>= putStr
    putStrLn " -h' to get more help"
    exitWith (ExitFailure 1))
  opts <- foldl (>>=) (return startOptions) actions
  command <- optCommand opts
  if not (null command)
    then return command
    else if null nonOptions
    then getContents
    else readFile (head nonOptions)

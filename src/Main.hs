module Main (main) where
  import Cli

  main = do
    s <- getInputProgram
    putStrLn s

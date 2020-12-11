module Main (main) where
import Cli

main :: IO()
main = do
  s <- getInputProgram
  putStrLn s

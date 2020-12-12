module Main (main) where
import Cli
import Parsers (eval)

main :: IO()
main = do
  s <- getInputProgram
  print $ eval s

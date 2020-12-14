module Main (main) where
import           Cli
import           Parser (eval)

main :: IO()
main = do
  s <- getInputProgram
  print $ eval s

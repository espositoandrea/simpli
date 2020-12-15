module Parser.Com (program) where

import           Control.Applicative
import           Control.Monad       hiding (return)
import           Environment
import           Parser.Aexp
import           Parser.Bexp
import           Parser.Core
import           Parser.Environment
import           Parser.Fundamentals
import qualified Parser.Readers      as R

program :: Parser String
program = (do command
              symbol ";"
              program) <|>
          (do command
              symbol ";")
          <|> command

command :: Parser String
command = assignment <|> ifThenElse <|> while <|> (symbol "skip")


assignment :: Parser String
assignment = do var <- identifier
                symbol ":="
                val <- aexp
                updateEnv Variable{name=var, vtype="int", value=val}


ifThenElse :: Parser String
ifThenElse = do symbol "if "
                b <- bexp
                symbol " then "
                if b
                  then (do x <- program
                           R.parseElse)
                  else (do R.program
                           parseElse)
                return ""

parseElse :: Parser String
parseElse = (do symbol "else "
                program
                symbol "end")
            <|>
            symbol "end"

while :: Parser String
while = do r <- R.while
           repeatWhile r
           symbol "while "
           b <- bexp
           symbol " do "
           if b
             then do program
                     symbol "end"
                     repeatWhile r
                     while
             else do R.program
                     symbol "end"
                     return ""

repeatWhile :: String -> Parser String
repeatWhile s = P(\env input -> Just (env, "", s ++ input))

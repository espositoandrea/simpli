module Parsers.Com (program) where

import           Control.Applicative
import           Control.Monad        hiding (return)
import           Environment
import           Parsers.Aexp
import           Parsers.Bexp
import           Parsers.Core
import           Parsers.Environment
import           Parsers.Fundamentals
import qualified Parsers.Readers      as R
import           Prelude              hiding (return)

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
repeatWhile s = P(\env input -> [(env, "", s ++ input)])

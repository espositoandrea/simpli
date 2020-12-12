module Parsers.Aexp (aexp) where

import           Control.Applicative
import           Parsers.Core
import           Parsers.Environment
import           Parsers.Fundamentals
import           Prelude              hiding (return)

aexp :: Parser Int
aexp = (do t <- aterm
           symbol "+"
           a <- aexp
           return (t + a))
       <|>
       (do t <- aterm
           symbol "-"
           a <- aexp
           return (t - a))
       <|> aterm

aterm :: Parser Int
aterm = (do f <- afactor
            symbol "*"
            a <- aterm
            return (f * a))
        <|> afactor

afactor :: Parser Int
afactor = (do symbol "("
              a <- aexp
              symbol ")"
              return a)
          <|>
          (do i <- identifier
              readVariable i)
          <|> integer

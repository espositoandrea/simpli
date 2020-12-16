module Parser.Aexp (aexp, array) where

import           Control.Applicative
import           Parser.Core
import           Parser.Environment  (readArray, readVariable)
import           Parser.Fundamentals (identifier, integer, symbol)

aexp :: Parser Int
aexp = do t <- aterm
          symbol "+"
          a <- aexp
          return (t + a)
       <|> do t <- aterm
              symbol "-"
              a <- aexp
              return (t - a)
       <|> aterm

aterm :: Parser Int
aterm = do f <- afactor
           symbol "*"
           a <- aterm
           return (f * a)
        <|> afactor

afactor :: Parser Int
afactor = do symbol "("
             a <- aexp
             symbol ")"
             return a
          <|> do symbol "|"
                 a <- array
                 symbol "|"
                 return $ length a
          <|> do a <- identifier
                 symbol "["
                 i <- aexp
                 symbol "]"
                 readVariable $ a ++ "[" ++ (show i) ++ "]"
          <|> do i <- identifier
                 readVariable i
          <|> integer

array :: Parser [Int]
array = do a <- basicarray
           symbol "++"
           b <- array
           return $ a ++ b
        <|> basicarray

basicarray :: Parser [Int]
basicarray = do symbol "["
                s <- asequence
                symbol "]"
                return s
             <|> do i <- identifier
                    readArray i

asequence :: Parser [Int]
asequence = do a <- aexp
               symbol ","
               as <- asequence
               return $ [a] ++ as
            <|> do a <- aexp
                   return [a]

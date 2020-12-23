module Parser.Aexp (aexp, array,cexp) where

import           Control.Applicative
import           Parser.Core
import           Parser.Environment  (readArray, readVariable)
import           Parser.Fundamentals (identifier, integer, symbol, satisfies)

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
          <|> do symbol "|"
                 a <- cexp
                 symbol "|"
                 return $ length a
          <|> do a <- identifier
                 symbol "["
                 i <- aexp
                 symbol "]"
                 v <- readVariable $ a ++ "[" ++ (show i) ++ "]"
                 case v of
                   Left v  -> return v
                   Right v -> empty
          <|> do i <- identifier
                 v <- readVariable i
                 case v of
                   Left v  -> return v
                   Right v -> empty
          <|> integer

-- ARRAYS

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

-- STRINGS

cexp :: Parser String
cexp = do x <- cterm
          symbol "++"
          xs <- cexp
          return $ x ++ xs
       <|> do x <- cterm
              symbol "["
              i <- aexp
              symbol "]"
              return $ [x !! i]
       <|> cterm

cterm :: Parser String
cterm = do symbol "\""
           x <- string
           symbol "\""
           return x
        <|> do i <- identifier
               v <- readVariable i
               case v of
                 Left v  -> empty
                 Right v -> return v

string :: Parser String
string = do c <- satisfies (/='"')
            cs <- string
            return (c:cs)
         <|> do x <- satisfies (/='"')
                return [x]

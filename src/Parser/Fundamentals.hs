module Parser.Fundamentals where

import           Control.Applicative
import           Data.Char           (digitToInt, isDigit, isLower, isUpper)
import Data.List
import           Parser.Core

-- |The parser "item" fails if the input is empty and consumes the first
-- character otherwise
item :: Parser Char
item = P(\env input -> case input of
           []     -> Nothing
           (x:xs) -> Just (env, x, xs))

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do x <- item
                 if p x
                   then return x
                   else empty

symbol :: String -> Parser String
symbol [] = return ""
symbol (x:xs) = do satisfies (x ==)
                   symbol xs
                   return (x:xs)

digit :: Parser Int
digit = do x <- satisfies isDigit
           return $ digitToInt x

natural :: Parser Int
natural = (do d <- digit
              n <- natural
              return (read (show d ++ show n) ::Int))
          <|> digit

integer :: Parser Int
integer = (do symbol "-"
              y <- natural
              return (- y))
          <|> natural

lower :: Parser Char
lower = satisfies isLower

upper :: Parser Char
upper = satisfies isUpper

alphanum :: Parser String
alphanum = (do x <- upper
               xs <- alphanum
               return (x:xs))
           <|>
           (do x <- lower
               xs <- alphanum
               return (x:xs))
           <|>
           (do x <- natural
               xs <- alphanum
               return $ (show x) ++ xs)
           <|>
           (do x <- natural
               return (show x))
           <|>
           (do x <- upper
               return [x])
           <|>
           (do x <- lower
               return [x])

identifier :: Parser String
identifier = (do x <- upper
                 xs <- alphanum
                 return (x:xs))
             <|>
             (do x <- lower
                 xs <- alphanum
                 return (x:xs))
             <|>
             (do x <- upper
                 return [x])
             <|>
             (do x <- lower
                 return [x])

notsymbol :: String -> Parser String
notsymbol [] = return ""
notsymbol xs = P(\env input ->
  case find  ((reverse xs) `isSuffixOf`) (inits . reverse $ input) of
    Nothing -> Just (env, input, "")
    Just i -> Just (env, reverse $ drop (length i) $ reverse input, reverse i))

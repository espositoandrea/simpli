module Parsers.Fundamentals where

import           Parsers.Base

-- |The parser "item" fails if the input is empty and consumes the first
-- character otherwise
item :: Parser Char
item = P(\env input -> case input of
           []     -> []
           (x:xs) -> [(env, x, xs)])

-- | A parser that always fails.
failure :: Parser a
failure = P(\_ _ -> [])

-- |A parser that always succeeds returning the value v without consuming any
-- input
return :: a -> Parser a
return v = P(\env input -> [(env, v, input)])

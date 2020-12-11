module Parsers.Base where
import           Environment

newtype Parser a = P(Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P par) env input = par [] input

-- Parser deve essere functor, applicative and monad
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P(\env input -> case parse p env input of
                []              -> []
                [(env, v, out)] -> [(env, g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P(\env input -> [(env, v, input)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pa <*> pb = P(\env input -> case parse pa env input of
                  []              -> []
                  [(env, f, out)] -> parse (fmap f pb) env out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\env input -> case parse p env input of
                []              -> []
                [(env, v, out)] -> parse (f v) env out)

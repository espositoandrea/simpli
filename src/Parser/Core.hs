module Parser.Core (Parser (..), parse) where
import           Control.Applicative
import           Environment

newtype Parser a = P(Env -> String -> Maybe (Env, a, String))

parse :: Parser a -> Env -> String -> Maybe (Env, a, String)
parse (P par) env input = par env input

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P(\env input -> case parse p env input of
                Nothing            -> Nothing
                Just (env, v, out) -> Just (env, g v, out))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P(\env input -> Just (env, v, input))

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P(\env input -> case parse pf env input of
                  Nothing            -> Nothing
                  Just (env, f, out) -> parse (fmap f pa) env input)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\env input -> case parse p env input of
                Nothing            -> Nothing
                Just (env, v, out) -> parse (f v) env out)

  -- return :: a -> Parser a
  return v = pure v

instance Alternative Parser where
  -- empty :: Parser a
  empty = P(\_ _ -> Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P(\env input -> case parse p env input of
                Nothing            -> parse q env input
                Just (env, v, out) -> Just (env, v, out))

module Parsers.Bexp (bexp) where
import           Control.Applicative
import           Parsers.Aexp         (aexp)
import           Parsers.Core
import           Parsers.Fundamentals
import qualified Parsers.Readers      as R
import           Prelude              hiding (return)

bexp :: Parser Bool
bexp = (do x <- bterm
           symbol "or"
           if x
             then do R.bterm
                     return x
             else do y <- bterm
                     return $ x || y)
       <|> bterm

bterm :: Parser Bool
bterm = (do x <- bfactor
            symbol "and"
            if x
              then do y <- bterm
                      return $ x && y
              else do R.bterm
                      return x)
       <|> bfactor

bfactor :: Parser Bool
bfactor = (do symbol "!"
              b <- bfactor
              return $ not b)
          <|>
          (do symbol "("
              b <- bexp
              symbol ")"
              return b)
          <|>
          (do symbol "true"
              return True)
          <|>
          (do symbol "false"
              return False)
          <|> bcomparison

bcomparison :: Parser Bool
bcomparison = (do a <- aexp
                  symbol "="
                  b <- aexp
                  return $ a == b)
              <|>
              (do a <- aexp
                  symbol "<="
                  b <- aexp
                  return $ a <= b)

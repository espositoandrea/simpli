module Parser.Bexp (bexp) where
import           Control.Applicative
import           Parser.Aexp         (aexp, cexp)
import           Parser.Core
import           Parser.Fundamentals
import qualified Parser.Readers      as R

bexp :: Parser Bool
bexp = do x <- bterm
          symbol "or"
          if x
            then do R.bterm
                    return x
            else do y <- bterm
                    return $ x || y
       <|> bterm

bterm :: Parser Bool
bterm = do x <- bfactor
           symbol "and"
           if x
             then do y <- bterm
                     return $ x && y
             else do R.bterm
                     return x
        <|> bfactor

bfactor :: Parser Bool
bfactor = do symbol "!"
             b <- bfactor
             return $ not b
          <|> do symbol "("
                 b <- bexp
                 symbol ")"
                 return b
          <|> do symbol "true"
                 return True
          <|> do symbol "false"
                 return False
          <|> bcomparison

bcomparison :: Parser Bool
bcomparison = do a <- aexp
                 symbol "="
                 b <- aexp
                 return $ a == b
              <|> do a <- aexp
                     symbol "<="
                     b <- aexp
                     return $ a <= b
              <|> do a <- aexp
                     symbol "<"
                     b <- aexp
                     return $ a < b
              <|> do a <- cexp
                     symbol "="
                     b <- cexp
                     return $ a == b

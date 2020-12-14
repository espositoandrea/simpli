module Parser.Readers where
import           Control.Applicative
import           Parser.Core
import           Parser.Fundamentals
import           Prelude             hiding (return)

aexp :: Parser String
aexp = (do t <- aterm
           symbol "+"
           a <- aexp
           return $ t ++ "+" ++ a)
       <|>
       (do t <- aterm
           symbol "-"
           a <- aexp
           return $ t ++ "-" ++ a)
       <|> aterm

aterm :: Parser String
aterm = (do f <- afactor
            symbol "*"
            a <- aterm
            return $ f ++ "*" ++ a)
        <|> afactor

afactor :: Parser String
afactor = (do symbol "("
              a <- aexp
              symbol ")"
              return $ "(" ++ a ++ ")")
          <|>
          identifier
          <|> (do x <- integer
                  return $ "" ++ (show x))

bexp :: Parser String
bexp = (do x <- bterm
           symbol "or"
           y <- bexp
           return $ x ++ "||" ++ y)
       <|> bterm

bterm :: Parser String
bterm = (do x <- bfactor
            symbol "and"
            y <- bterm
            return $ x ++ "&&" ++ y)
       <|> bfactor

bfactor :: Parser String
bfactor = (do symbol "!"
              b <- bfactor
              return $ "!" ++ b)
          <|>
          (do symbol "("
              b <- bexp
              symbol ")"
              return $ "(" ++ b ++ ")")
          <|>
          symbol "true"
          <|>
          symbol "false"
          <|> bcomparison

bcomparison :: Parser String
bcomparison = (do a <- aexp
                  symbol "="
                  b <- aexp
                  return $ a ++ "==" ++ b)
              <|>
              (do a <- aexp
                  symbol "<="
                  b <- aexp
                  return $ a ++ "<=" ++ b)



command :: Parser String
command = assignment <|> ifThenElse <|> while
          <|> symbol "skip"

program :: Parser String
program = (do x <- command
              symbol ";"
              y <- program
              return $ x ++ ";" ++ y) <|>
          (do x <- command
              symbol ";"
              return $ x ++ ";")
          <|> command


assignment :: Parser String
assignment= do var <- identifier
               symbol ":="
               val <- aexp
               return $ var ++ ":=" ++ val

ifThenElse :: Parser String
ifThenElse = do symbol "if "
                b <- bexp
                symbol " then "
                x <- program
                y <- parseElse
                return $ "if " ++ b ++ " then " ++ x ++ y

parseElse :: Parser String
parseElse = (do symbol "else "
                x <- program
                symbol "end"
                return $ "else " ++ x ++ "end")
            <|>
            symbol "end"

while :: Parser String
while = do symbol "while "
           b <- bexp
           symbol " do "
           x <- program
           symbol "end"
           return $ "while " ++ b ++ " do " ++ x ++ "end"

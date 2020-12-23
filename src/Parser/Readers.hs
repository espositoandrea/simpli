module Parser.Readers where
import           Control.Applicative
import           Parser.Core
import           Parser.Fundamentals

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
          <|> do symbol "|"
                 a <- array
                 symbol "|"
                 return $ "|" ++ a ++ "|"
          <|> do symbol "|"
                 a <- cexp
                 symbol "|"
                 return $ "|" ++ a ++ "|"
          <|> do a <- identifier
                 symbol "["
                 i <- aexp
                 symbol "]"
                 return $ a ++ "[" ++ i ++ "]"
          <|> identifier
          <|> do x <- integer
                 return $ "" ++ (show x)

array :: Parser String
array = do a <- basicarray
           symbol "++"
           b <- array
           return $ a ++ "++" ++ b
        <|> basicarray

basicarray :: Parser String
basicarray = do symbol "["
                s <- asequence
                symbol "]"
                return $ "[" ++ s ++ "]"
             <|> identifier

asequence :: Parser String
asequence = do a <- aexp
               symbol ","
               as <- asequence
               return $ a ++ "," ++ as
            <|> aexp
-- STRINGS

cexp :: Parser String
cexp = do x <- cterm
          symbol "++"
          xs <- cexp
          return $ x ++ "++" ++ xs
       <|> do x <- cterm
              symbol "["
              i <- aexp
              symbol "]"
              return $ x ++ "[" ++ i ++ "]"
       <|> cterm

cterm :: Parser String
cterm = do symbol "\""
           x <- string
           symbol "\""
           return $ "\"" ++ x ++ "\""
        <|> identifier

string :: Parser String
string = do c <- satisfies (/='"')
            cs <- string
            return (c:cs)
         <|> do x <- satisfies (/='"')
                return [x]

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
bcomparison = do a <- aexp
                 symbol "="
                 b <- aexp
                 return $ a ++ "=" ++ b
              <|> do a <- aexp
                     symbol "<="
                     b <- aexp
                     return $ a ++ "<=" ++ b
              <|> do a <- aexp
                     symbol "<"
                     b <- aexp
                     return $ a ++ "<" ++ b
              <|> do a <- cexp
                     symbol "="
                     b <- cexp
                     return $ a ++ "=" ++ b


command :: Parser String
command = assignment <|> ifThenElse <|> while
          <|> symbol "skip"

program :: Parser String
program = do x <- command
             symbol ";"
             y <- program
             return $ x ++ ";" ++ y
          <|> do x <- command
                 symbol ";"
                 return $ x ++ ";"
          <|> command


assignment :: Parser String
assignment = do var <- identifier
                symbol ":="
                val <- P(\env input ->
                           let took = takeWhile (/=';') input
                           in Just (env, took, drop (length took) input))
                return $ var ++ ":=" ++ val
            <|> do arr <- identifier
                   symbol "["
                   i <- aexp
                   symbol "]"
                   symbol ":="
                   val <- aexp
                   return $ arr ++ "[" ++ i ++ "]:=" ++ val

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

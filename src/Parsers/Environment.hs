module Parsers.Environment where
import           Environment
import           Parsers.Base

updateEnv :: Variable -> Parser String
updateEnv var = P(\env input -> case input of
                   xs -> [(modifyEnv env var, "", xs)])

readVariable :: String -> Parser Int
readVariable name = P(\env input -> case searchVariable env name of
                       []      -> []
                       [value] -> [(env, value, input)])

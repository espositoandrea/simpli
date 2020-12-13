module Parsers.Environment (updateEnv, readVariable) where
import           Environment
import           Parsers.Core

updateEnv :: Variable -> Parser String
updateEnv var = P(\env input -> Just (modifyEnv env var, "", input))

readVariable :: String -> Parser Int
readVariable name = P(\env input -> case searchVariable env name of
                       Nothing    -> Nothing
                       Just value -> Just (env, value, input))

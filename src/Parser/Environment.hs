module Parser.Environment
    (
      updateEnv
    , readVariable
    , readArray
    , saveArray
    ) where

import           Environment (Variable, modifyArray, modifyEnv, searchArray,
                              searchVariable)
import           Parser.Core (Parser (..))

updateEnv :: Variable -> Parser String
updateEnv var = P(\env input -> Just (modifyEnv env var, "", input))

readVariable :: String -> Parser (Either Int String)
readVariable name = P(\env input -> case searchVariable env name of
                       Nothing    -> Nothing
                       Just value -> Just (env, value, input))

saveArray :: String -> [Int] -> Parser String
saveArray var val = P(\env input -> Just (modifyArray env var val, "", input))

readArray :: String -> Parser [Int]
readArray name = P(\env input -> case searchArray env name of
                    Nothing -> Nothing
                    Just xs -> Just (env, xs, input))

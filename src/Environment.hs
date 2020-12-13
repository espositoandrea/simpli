module Environment (Variable(..), Env, modifyEnv, searchVariable) where

data Variable = Variable { name  :: String
                         , vtype :: String
                         , value :: Int
                         }

instance Show Variable where
  -- show :: a -> String
  show var = (name var) ++ " = " ++ show (value var)

type Env = [Variable]

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) var
  | (name x) == (name var) = [var] ++ xs
  | otherwise              = [x] ++ modifyEnv xs var

searchVariable :: Env -> String -> Maybe Int
searchVariable [] _ = Nothing 
searchVariable (x:xs) query
  | (name x) == query = Just $ value x
  | otherwise         = searchVariable xs query

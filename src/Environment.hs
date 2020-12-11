module Environment where

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

searchVariable :: Env -> String -> [Int]
searchVariable [] _ = []
searchVariable (x:xs) query
  | (name x) == query = [value x]
  | otherwise         = searchVariable xs query

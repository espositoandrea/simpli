module Environment
    (
      Variable(..)
    , Env
    , modifyEnv
    , searchVariable
    , modifyArray
    , searchArray
    ) where

data Variable = Variable
    { name  :: !String
    , vtype :: !String
    , value :: Either Int String
    } deriving (Eq)

instance Show (Variable) where
  show var = (name var) ++ " = " ++ val
                where val = case value var of
                              Left v  -> show v
                              Right v -> show v

type Env = [Variable]

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) var
  | (name x) == (name var) = [var] ++ xs
  | otherwise              = [x] ++ modifyEnv xs var

searchVariable :: Env -> String -> Maybe (Either Int String)
searchVariable [] _ = Nothing
searchVariable (x:xs) query
  | (name x) == query = Just $ value x
  | otherwise         = searchVariable xs query

modifyArray :: Env -> String -> [Int] -> Env
modifyArray env var val = foldl (modifyEnv) env l
                          where l = zipWith (\a i ->
                                   Variable { name=var ++ "[" ++ (show i) ++ "]"
                                            , vtype="int[]"
                                            , value=Left a}) val [0..]

searchArray :: Env -> String -> Maybe [Int]
searchArray env name =
  case f 0 of
       []    -> Nothing
       value -> Just value
  where f i = case searchVariable env (name ++ "[" ++ (show i) ++ "]") of
              Nothing            -> []
              Just (Right value) -> []
              Just (Left value)  -> [value] ++ f (i + 1)

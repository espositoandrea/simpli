module Parser (eval) where

import           Data.List
import           Environment (Env)
import           Parser.Com  (program)
import           Parser.Core (parse)

removeComments :: String -> String
removeComments [] = []
removeComments xs = unlines $ map f (lines xs)
                      where
                        f [] = []
                        f (x:xs)
                          | x == '#'  = []
                          | otherwise = x:f xs

removeWhitespace :: String -> String
removeWhitespace [] = []
removeWhitespace xs = foldl manageExceptions "" (words xs)
  where
    manageExceptions = \x y ->
      if (any (`isPrefixOf` y) ["else", "end"]) && not (";" `isSuffixOf` x)
        then x ++ ";" ++ y
        else if any (`isSuffixOf` x) ["while", "if", "else"]
        then x ++ " " ++ y
        else if any (`isPrefixOf` y) ["do","then"]
        then x ++ " " ++ y ++ " "
        else if odd $ (length . filter (=='"')) x -- We're inside a string
        then x ++ " " ++ y
        else x ++ y

eval :: String -> Env
eval c = if null cleanedc
           then []
           else case parse program [] cleanedc of
             Nothing          -> error "Invalid input"
             Just (e, _, [])  -> e
             Just (e, _, out) -> error $ "Invalid input: unused '" ++ out ++ "'"
         where cleanedc = (removeWhitespace . removeComments) c

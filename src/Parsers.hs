module Parsers (eval) where

import           Data.List
import           Environment  (Env)
import           Parsers.Com  (program)
import           Parsers.Core (parse)

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
removeWhitespace xs = foldl f "" (words xs)
                        where
                          f = \x y -> if (any (`isPrefixOf` y) ["else", "end"])
                                          && not (";" `isPrefixOf` x)
                                        then x ++";"++y
                                        else if any (`isSuffixOf` x) ["while", "if", "else"]
                                        then x ++ " " ++ y
                                        else if any (`isPrefixOf` y) ["do","then"]
                                        then x ++ " " ++ y ++ " "
                                        else x ++ y
eval :: String -> Env
eval c = case parse program [] ((removeWhitespace . removeComments) c) of
           Nothing          -> error "Invalid input"
           Just (e, _, [])  -> e
           Just (e, _, out) -> error $ "Invalid input: unused '" ++ out ++ "'"


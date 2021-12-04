{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

calculatePosition :: [(String, Int)] -> Int -> Int -> Int
calculatePosition (("forward", x):commands) pos depth = calculatePosition commands (pos+x) depth
calculatePosition (("up", x):commands) pos depth = calculatePosition commands pos (depth-x)
calculatePosition (("down", x):commands) pos depth = calculatePosition commands pos (depth+x)
calculatePosition (_:commands) pos depth = calculatePosition commands pos depth
calculatePosition [] pos depth = pos * depth

getInput :: [Char] -> IO [String]
getInput filename = fmap lines (readFile filename)

splitAndParse :: [String] -> (String, Int)
splitAndParse [x,y] = (x, toInt y)
splitAndParse _ = ("X", 0)

parse :: [String] -> [(String, Int)]
parse = map (splitAndParse . words)

toInt :: String -> Int
toInt x = read x :: Int

main = do 
   ls <- fmap parse (getInput "input")
   print $ calculatePosition ls 0 0


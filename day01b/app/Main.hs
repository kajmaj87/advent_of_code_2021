module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Attoparsec.Text
import Data.Either

import Lib

slideWindow n xs 
    | length xs > n = sum (Prelude.take n xs) : slideWindow n (tail xs)
    | otherwise = []

slide :: (Num a) => [a] -> [a]
slide = slideWindow 3

f :: (Ord a, Num a) => [a] -> [a]
f (x:y:z) = (if x < y then 1 else 0) : f (y:z)
f [x] = []
f [] = []

toInt = parseOnly (signed decimal)

main = do 
   ls <- fmap Text.lines (Text.readFile "input")
   print $ sum $ f $ slide (rights (map toInt ls))



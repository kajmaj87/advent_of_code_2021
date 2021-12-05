module Main where

import Data.List

powerRate :: [[Char]] -> Int
powerRate xs = fromBinary (oxygen xs) * fromBinary (co2 xs)

fromBinary :: [Char] -> Int
fromBinary xs = binary (reverse xs) 1
  where
    binary ('1' : ys) pow = pow + binary ys 2 * pow
    binary ('0' : ys) pow = binary ys 2 * pow
    binary _ _ = 0

oxygen :: [[Char]] -> [Char]
oxygen xs = undefined

filterByBit :: Int -> Char -> [[Char]] -> [[Char]]
filterByBit pos c = filter (nthMatch pos c)
  where
    nthMatch pos c number = number !! pos == c

co2 :: [[Char]] -> [Char]
co2 = undefined

mostCommonBit :: [Char] -> Char
mostCommonBit xs
  | 2 * count xs >= length xs = '1'
  | otherwise = '0'

leastCommonBit :: [Char] -> Char
leastCommonBit xs
  | 2 * count xs <= length xs = '0'
  | otherwise = '1'

count :: [Char] -> Int
count xs = length (filter (== '1') xs)

getInput :: [Char] -> IO [[Char]]
getInput filename = fmap lines (readFile filename)

main = do
  ls <- getInput "input"
  print $ powerRate ls

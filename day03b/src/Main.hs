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
oxygen = findLast mostCommonBit

co2 :: [[Char]] -> [Char]
co2 = findLast leastCommonBit

findLast :: ([Char] -> Char) -> [[Char]] -> [Char]
findLast commonBitFunction xs = findLast' xs 0
  where
    findLast' xs n
      | length xs > 1 = findLast' (filterByBit n (commonBitFunction (slice xs n)) xs) (n + 1)
      | otherwise = head xs

slice :: [[Char]] -> Int -> [Char]
slice xs n = transpose xs !! n

filterByBit :: Int -> Char -> [[Char]] -> [[Char]]
filterByBit pos c = filter (nthMatch pos c)
  where
    nthMatch pos c number = number !! pos == c

mostCommonBit :: [Char] -> Char
mostCommonBit xs
  | 2 * count xs >= length xs = '1'
  | otherwise = '0'

leastCommonBit :: [Char] -> Char
leastCommonBit xs
  | 2 * count xs >= length xs = '0'
  | otherwise = '1'

count :: [Char] -> Int
count xs = length (filter (== '1') xs)

-- parsing
getInput :: [Char] -> IO [[Char]]
getInput filename = fmap lines (readFile filename)

main = do
  ls <- getInput "input"
  print $ powerRate ls

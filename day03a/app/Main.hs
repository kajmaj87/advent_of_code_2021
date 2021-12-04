module Main where

import Data.List

powerRate :: [[Char]] -> Int
powerRate xs = fromBinary (gamma xs) * fromBinary (epsilon xs)

fromBinary :: [Char] -> Int
fromBinary xs = binary (reverse xs) 1
    where binary ('1':ys) pow = pow + binary ys 2*pow
          binary ('0':ys) pow = binary ys 2*pow
          binary _ _ = 0

gamma :: [[Char]] -> [Char]
gamma xs = map get0or1 (transpose xs)

epsilon :: [[Char]] -> [Char]
epsilon = negateNumber . gamma

negateNumber :: [Char] -> [Char]
negateNumber = map flip
  where flip '1' = '0'
        flip '0' = '1'
        flip _ = 'X'

get0or1 :: [Char] -> Char
get0or1 xs 
    | 2 * count xs > length xs = '1'
    | otherwise = '0'

count :: [Char] -> Int
count xs = length (filter (=='1') xs)



getInput :: [Char] -> IO [[Char]]
getInput filename = fmap lines (readFile filename)


main = do 
   ls <- getInput "input"
   print $ powerRate ls

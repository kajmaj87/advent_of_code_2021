module Main where

import GHC.Fingerprint (fingerprint0)

minCost :: [Int] -> Int
minCost xs = minimum (map sum (allCosts xs))

allCosts :: [Int] -> [[Int]]
allCosts ys = map (cost ys) [minimum ys .. maximum ys]

cost :: [Int] -> Int -> [Int]
cost xs n = map (moveCost n) xs

moveCost :: Int -> Int -> Int
moveCost a b
  | a == b = 0
  | otherwise = c * (c + 1) `div` 2
  where
    c = abs (a - b)

-- parsing
toInt :: [String] -> [Int]
toInt = map (\x -> read x :: Int)

main :: IO ()
main = do
  ls <- toInt . words . head . lines <$> readFile "input"
  print $ minCost ls

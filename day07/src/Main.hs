module Main where

minCost :: [Int] -> Int
minCost xs = minimum (map sum (allCosts xs))

allCosts :: [Int] -> [[Int]]
allCosts ys = map (cost ys) [minimum ys .. maximum ys]

cost :: [Int] -> Int -> [Int]
cost xs n = map (\x -> abs (x - n)) xs

-- parsing
toInt :: [String] -> [Int]
toInt = map (\x -> read x :: Int)

main :: IO ()
main = do
  ls <- toInt . words . head . lines <$> readFile "input"
  print $ minCost ls

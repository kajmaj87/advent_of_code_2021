module Main where

simulateFishBreeding :: [Int] -> Int -> [Int]
simulateFishBreeding fs days
  | days > 0 = decreaseTimer yesterdaysFish ++ breedFishes yesterdaysFish
  | otherwise = fs
  where
    yesterdaysFish = simulateFishBreeding fs (days - 1)

decreaseTimer :: [Int] -> [Int]
decreaseTimer = map (\x -> if x == 0 then 6 else x - 1)

breedFishes :: [Int] -> [Int]
breedFishes fs = map (const 8) (filter (== 0) fs)

toInt x = read x :: Int

main :: IO ()
main = do
  ls <- fmap lines (readFile "input")
  print $ length (simulateFishBreeding (map toInt (words (head ls))) 80)

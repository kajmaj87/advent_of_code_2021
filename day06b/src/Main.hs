module Main where

type Fishes = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

result :: Fishes -> Int -> Int
result fs days = a + b + c + d + e + f + g + h + i
  where
    (a, b, c, d, e, f, g, h, i) = simulateFishBreeding fs days

simulateFishBreeding :: Fishes -> Int -> Fishes
simulateFishBreeding fs days
  | days > 0 = decreaseTimerAndBreed yesterdaysFish
  | otherwise = fs
  where
    yesterdaysFish = simulateFishBreeding fs (days - 1)

decreaseTimerAndBreed :: Fishes -> Fishes
decreaseTimerAndBreed (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h + a, i, a)

-- parsing code below
toInt :: String -> Int
toInt x = read x :: Int

unsafeMap :: [Int] -> Fishes
unsafeMap fs = (f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8)
  where
    f = getFishCount fs

getFishCount :: [Int] -> Int -> Int
getFishCount fs n = length (filter (== n) fs)

main :: IO ()
main = do
  ls <- fmap lines (readFile "input")
  print $ result (unsafeMap (map toInt (words (head ls)))) 256

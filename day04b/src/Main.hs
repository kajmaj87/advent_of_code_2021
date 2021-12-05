module Main where

import Data.List

type Bingo = [[Int]]

type Lottery = [Int]

score :: [Bingo] -> Lottery -> Int
score bs l = cost (lastWinner bs l) (lastWinNumbers bs l) * last (lastWinNumbers bs l)

cost :: Bingo -> Lottery -> Int
cost bs l = sum $ concat bs \\ l

lastWinNumbers :: [Bingo] -> Lottery -> [Int]
lastWinNumbers bs l = findWinningNumbers bs l 0
  where
    findWinningNumbers bs l n = if length bs == 1 && win (head bs) (take n l) then take n l else findWinningNumbers (losers bs (take n l)) l (n + 1)

lastWinner :: [Bingo] -> Lottery -> Bingo
lastWinner bs l = findWinner bs l 0
  where
    findWinner bs l n
      | length bs == 1 = head bs
      | otherwise = findWinner (losers bs (take n l)) l (n + 1)

losers :: [Bingo] -> Lottery -> [Bingo]
losers bs l = filter (\b -> not (win b l)) bs

win :: Bingo -> Lottery -> Bool
win b l = horizontalWin b l || horizontalWin (transpose b) l

horizontalWin :: Bingo -> Lottery -> Bool
horizontalWin b l = any (\xs -> null (xs \\ l)) b

-- parsing code
toInt :: String -> Int
toInt x = read x :: Int

groupsOfN :: Int -> [a] -> [[a]]
groupsOfN _ [] = []
groupsOfN n l
  | n > 0 = take n l : groupsOfN n (drop n l)
  | otherwise = error "Negative or zero n"

main :: IO ()
main = do
  ls <- fmap lines (readFile "input")
  let lottery = map toInt (words (head ls))
  let rest = tail ls
  let bingos = groupsOfN 5 (filter (not . null) (map (map toInt . words) rest))
  print $ score bingos lottery

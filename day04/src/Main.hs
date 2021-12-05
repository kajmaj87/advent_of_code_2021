module Main where

import Data.List

type Bingo = [[Int]]

type Lottery = [Int]

score :: [Bingo] -> Lottery -> Int
score bs l = cost (winner bs l) (winningNumbers bs l) * last (winningNumbers bs l)

cost :: Bingo -> Lottery -> Int
cost bs l = sum $ concat bs \\ l

winningNumbers :: [Bingo] -> Lottery -> [Int]
winningNumbers bs l = findWinningNumbers bs l 0
  where
    findWinningNumbers bs l n = if any (\b -> win b (take n l)) bs then take n l else findWinningNumbers bs l (n + 1)

winner :: [Bingo] -> Lottery -> Bingo
winner bs l = findWinner bs l 0
  where
    findWinner bs l n = case find (\b -> win b (take n l)) bs of
      Just b -> b
      _ -> findWinner bs l (n + 1)

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
  let bingos = groupsOfN 5 (filter (not . null) (map (map toInt) (map words rest)))
  print $ score bingos lottery

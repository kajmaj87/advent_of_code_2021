module Main where

import Data.Map.Strict hiding (map)
import Prelude hiding (filter)

type Point = (Int, Int)

type Line = (Point, Point)

type Lines = [Line]

type DangerValue = (Point, Int)

type DangerMap = Map Point Int

dangerValue :: Lines -> Int
dangerValue ls = size (filter (>= 2) (calculateDanger ls))

calculateDanger :: Lines -> DangerMap
calculateDanger ls = applyDanger ls empty
  where
    applyDanger (l : ls) m = applyDanger ls (applyLine l m)
    applyDanger [] m = m

applyLine :: Line -> DangerMap -> DangerMap
applyLine l m = unionWith (+) m (dangerFromLine l)

dangerFromLine :: Line -> DangerMap
dangerFromLine ((x1, y1), (x2, y2))
  | x1 == x2 = fromList [((x1, y), 1) | y <- [(min y1 y2) .. (max y1 y2)]]
  | y1 == y2 = fromList [((x, y1), 1) | x <- [(min x1 x2) .. (max x1 x2)]]
  | x1 + x2 == y1 + y2 = dangerFromDiagonal ((x1, y1), (x2, y2))
  | otherwise = empty

dangerFromDiagonal :: Line -> DangerMap
dangerFromDiagonal ((x1, y1), (x2, y2))
  | x1 < x2 && y1 < y2 = fromList [((x1 + d, y1 + d), 1) | d <- [0 .. (x2 - x1)]]
  | x1 > x2 && y1 < y2 = fromList [((x1 - d, y1 + d), 1) | d <- [0 .. (x1 - x2)]]
  | x1 < x2 && y1 > y2 = fromList [((x1 + d, y1 - d), 1) | d <- [0 .. (x2 - x1)]]
  | x1 > x2 && y1 > y2 = fromList [((x1 - d, y1 - d), 1) | d <- [0 .. (x1 - x2)]]
  | otherwise = undefined

-- parsing code
toInt :: String -> Int
toInt x = read x :: Int

unsafeToLine :: [Int] -> Line
unsafeToLine [a, b, c, d] = ((a, b), (c, d))
unsafeToLine _ = undefined

main :: IO ()
main = do
  ls <- fmap lines (readFile "input")
  let lines = map (unsafeToLine . map toInt . words) ls
  print $ dangerValue lines

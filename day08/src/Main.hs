module Main where

count :: ([String], [String]) -> Int
count (a, b) = length $ filter (\x -> length x `elem` [2, 4, 3, 7]) b

split :: String -> [String] -> ([String], [String])
split c ss = (takeWhile (/= c) ss, filter (/= c) $ dropWhile (/= c) ss)

main :: IO ()
main = do
  ls <- lines <$> readFile "input"
  let result = map (count . split "|" . words) ls
  print $ sum result

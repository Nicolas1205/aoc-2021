module Main where

import Data.Char
import System.Environment
import Text.Printf

sum' :: [String] -> Int -> String
sum' xs i = map (!! i) xs

sumRow :: String -> Int
sumRow xs = sumRow' xs 0 0
  where
    sumRow' [] zeros ones
      | zeros > ones = 0
      | otherwise = 1
    sumRow' (x : xs) zeros ones
      | x == '0' = sumRow' xs (zeros + 1) ones
      | x == '1' = sumRow' xs zeros (ones + 1)

filterMax :: [String] -> Int -> [String]
filterMax xs i = filter (\s -> digitToInt (head s) == max) xs
  where
    max = sumRow $ sum' xs i

solve :: [String] -> String
solve xs = solve' xs (length $ head xs) 0 ""
  where
    solve' xs n i last'
      | length xs < 3 = last'
      | i <= n = solve' nxs n (i + 1) last'
      | otherwise = last'
      where
        nxs = filterMax xs i
        last' = last xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath : _) -> do
      xs <- lines <$> readFile filePath
      print $ solve xs
    _ -> error "File input not provided"

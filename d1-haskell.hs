module Main where

import System.Environment
import Text.Printf

sumX :: [Int] -> Int
sumX xs = sum $ take 3 xs

solve :: [Int] -> Int -> Int
solve xs wsum
  | length xs < 3 = wsum
  | w1 < w2 = solve (drop 1 xs) (wsum + 1)
  | w1 >= w2 = solve (drop 1 xs) wsum
  where
    w1 :: Int
    w1 = sumX xs
    w2 :: Int
    w2 = sumX $ drop 1 xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath : _) -> do
      printf "Input file: %s\n" filePath
      n <- lines <$> readFile filePath
      let arr = map read n :: [Int]
      printf "Answer: %d\n" $ solve arr 0
    _ -> error "Input file is not provided"

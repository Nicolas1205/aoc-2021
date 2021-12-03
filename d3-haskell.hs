module Main where

import System.Environment
import Text.Printf

sum' :: [String] -> String
sum' = map head

sumRow :: String -> [Int]
sumRow xs = sumRow' xs 0 0
  where
    sumRow' (x : xs) zeros ones
      | x == 'e' = ans
      | x == '0' = sumRow' xs (zeros + 1) ones
      | x == '1' = sumRow' xs zeros (ones + 1)
      where
        ans
          | zeros > ones = [0]
          | otherwise = [1]

quitRow :: [String] -> [String]
quitRow = map tail

sumRows :: Int -> [String] -> [Int]
sumRows n xs = sumRows' n xs 0 []
  where
    sumRows' n (x : xs) i ans
      | x == "e" = ans

--      | i < n = sumRows' n (k)
--   | i < n = sumRows' n xs (i+1) (ans ++ [sumRow' $ sum' xs])

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath : _) -> do
      xs <- lines <$> readFile filePath
      print $ sumRow $ sum' xs ++ "e"
    --      print $ quitRow $ xs ++ (replicate (length xs) 'e')
    _ -> error "Input file not provided"

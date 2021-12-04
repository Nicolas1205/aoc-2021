module Main where

import Graphics.X11.Xinerama (XineramaScreenInfo (xsi_y_org))
import System.Environment
import Text.Printf

sum' :: [String] -> String
sum' = map head

sumRow :: String -> Int
sumRow xs = sumRow' xs 0 0
  where
    sumRow' (x : xs) zeros ones
      | x == 'e' = ans
      | x == '0' = sumRow' xs (zeros + 1) ones
      | x == '1' = sumRow' xs zeros (ones + 1)
      where
        ans
          | zeros > ones = 0
          | otherwise = 1

quitRow :: [String] -> [String]
quitRow xs
  | null xs = ["e"]
  | otherwise = map tail xs

sumRows :: Int -> [String] -> [Int]
sumRows n xs = sumRows' n xs 0 []
  where
    sumRows' n (x : xs) i ans
      | null xs || null x = ans
      | head' == 'e' = ans
      | i < n = sumRows' n (quitRow xs ++ [x]) (i + 1) (ans ++ [sumRow $ sum' xs])
      where
        head' = head x

solve :: [String] -> Int
solve x = (convert $ reverse ans) * (convert $ not' (ans ++ [2]))
  where
    ans = sumRows (length x) x

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs

not' :: [Int] -> [Int]
not' xs = not'' xs []
  where
    not'' (x : xs) ans
      | x == 2 = ans
      | x == 0 = not'' xs ans ++ [1]
      | x == 1 = not'' xs ans ++ [0]

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath : _) -> do
      xs <- lines <$> readFile filePath
      let x = xs ++ [replicate ((length $ head xs)) 'e']
      print $ solve x
    _ -> error "Input file not provided"

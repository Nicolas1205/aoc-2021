import Data.Char
import System.Environment
import Text.Printf

sum'' :: [String] -> Int
sum'' xs = helper xs 0 0 0
  where
    helper (x : xs) dy dx d
      | head' == 'e' = d * dx
      | head' == 'f' = helper xs dy (dx + last') (d + last' * dy)
      | head' == 'd' = helper xs (dy + last') dx d
      | head' == 'u' = helper xs (dy - last') dx d
      where
        head' = head x
        last' = digitToInt $ last x

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath : _) -> do
      printf "Input file: %s\n" filePath
      xs <- lines <$> readFile filePath
      print $ sum'' (xs ++ ["e"])
    _ -> error "Input file is not provided"

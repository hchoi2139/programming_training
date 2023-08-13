import Data.Char (digitToInt)

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  let ns = map digitToInt s
  print $ sum ns
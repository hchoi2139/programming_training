import Data.Char (ord)

main :: IO ()
main = do
  input <- getLine
  let l = head input
  print $ ord l
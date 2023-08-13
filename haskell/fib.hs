import Data.Array ( Ix(range), Array, (!), array )

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

fib :: Int -> Integer
fib n = table ! n
  where
    table :: Array Int Integer
    table = tabulate (0, n) memo

    memo 0 = 0
    memo 1 = 1
    memo n = table ! (n - 1) + table ! (n - 2)

main :: IO ()
main = do
  input <- readLn :: IO Int
  print (fib input)
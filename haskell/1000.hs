main :: IO ()
main = do
  input <- getLine
  let [a, b] = map read (words input) :: [Int]
  let sum = a + b
  print sum
main :: IO ()
main = do 
  input <- getLine
  let [a, b] = read <$> words input :: [Int]
  putStrLn $ cmp a b

cmp :: Int -> Int -> String
cmp a b
  | a > b   = ">"
  | a == b  = "=="
  | a < b   = "<"
  | otherwise = error "Somoething magical happened"
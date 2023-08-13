import Control.Monad (sequence)

main :: IO ()
main = do
  line <- getLine
  let [a, b] = read <$> words line :: [Int]
  print (a + b)
  main
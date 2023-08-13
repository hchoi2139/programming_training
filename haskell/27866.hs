main :: IO ()
main = do
  s <- getLine
  i <- readLn :: IO Int 
  putChar $ s !! (i - 1)
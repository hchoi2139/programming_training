main :: IO ()
main = do
  t <- readLn :: IO Int
  readNPrint

readNPrint :: IO ()
readNPrint = do
  s <- getLine
  putStrLn $ head s : [last s]
  readNPrint
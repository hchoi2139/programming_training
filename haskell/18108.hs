main :: IO ()
main = do 
  input <- getLine
  let year = read input :: Int
  print $ convertToAnnoDomini year

convertToAnnoDomini :: Int -> Int
convertToAnnoDomini year = year - 543
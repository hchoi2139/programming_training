main :: IO ()
main = do
  (h, m) <- getCurrentTime
  t <- getCookTime
  let (eh, em) = getEndCookTime h m t
  putStrLn $ show eh ++ " " ++ show em

getCurrentTime :: IO (Int, Int)
getCurrentTime = do
  input <- getLine
  let [h, m] = map read (words input) :: [Int]
  return (h, m)

getCookTime :: IO Int
getCookTime = do
  input <- getLine
  let cookTime = read input :: Int
  return cookTime

getEndCookTime :: Int -> Int -> Int -> (Int, Int)
getEndCookTime h m t = (h', m')
  where
    h'' = h + (t `div` 60)
    m'' = m + (t `mod` 60)
    h'  = (h'' + (m'' `div` 60)) `mod` 24
    m'  = m'' `mod` 60
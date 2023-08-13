main :: IO ()
main = do
  n <- getSize
  ns <- getList
  t <- getTarget
  let cnt = length $ filter (== t) ns
  print cnt

getSize :: IO Int
getSize = do
  input <- getLine
  let n = read input :: Int
  return n

getList :: IO [Int]
getList = do
  input <- getLine
  let list = map read (words input) :: [Int]
  return list

getTarget :: IO Int
getTarget = do
  input <- getLine
  let t = read input :: Int
  return t
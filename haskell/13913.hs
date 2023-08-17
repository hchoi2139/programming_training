import Control.Monad.ST  ( ST, runST )
import Data.Array.ST     ( Ix,  STArray, readArray, writeArray, MArray (newArray), freeze )
import Control.Monad (forM_)
import Data.Array (Array, (!))

maxVal :: Int
maxVal = 200000

findMinDist :: Int -> Int -> (Int, Array Int Int)
findMinDist n k = runST $ do
  dist <- newSTArray (0, maxVal) (-1)
  writeArray dist n 0
  let initDq = [n]
      accDq  = []
  minDist <- bfs k initDq accDq dist
  imDist  <- freeze dist
  return (minDist, imDist)

bfs :: Int -> [Int] -> [Int] -> STArray s Int Int -> ST s Int
bfs k [] acc dist  = bfs k acc [] dist
bfs k (t : ts) acc dist
  | t == k    = readArray dist t
  | otherwise = do
    curDist <- readArray dist t
    let validIdxs = filter isValidIdx [t * 2, t + 1, t - 1]
    candDists <- mapM (readArray dist) validIdxs
    let cands   = filter ((< 0) . snd) $ zip validIdxs candDists
        (is, _) = unzip cands
    forM_ is (\i -> writeArray dist i (curDist + 1))
    bfs k ts (acc ++ is) dist

isValidIdx :: Int -> Bool
isValidIdx i = 0 <= i && i <= maxVal

newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

findRoute :: Array Int Int -> Int -> Int -> [Int]
findRoute dist n k = findRoute' k []
  where
    findRoute' :: Int -> [Int] -> [Int]
    findRoute' i acc
      | i == n  = i : acc
      | i < maxVal && dist ! i == dist ! (i + 1) + 1
          = findRoute' (i + 1) (i : acc)
      | i > 0 && dist ! i == dist ! (i - 1) + 1
          = findRoute' (i - 1) (i : acc)
      | otherwise
          = findRoute' (i `div` 2) (i : acc)

main :: IO ()
main = do
  [n, k] <- fmap (map read . words) getLine :: IO [Int]
  print n
  print k
  let (minDist, dist) = findMinDist n k
      route = findRoute dist n k
  print minDist
  putStrLn $ unwords $ map show route
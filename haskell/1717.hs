import Control.Monad (replicateM, forM)
import Control.Monad.ST (ST, runST)
import Data.Array (Ix)
import Data.Array.ST (STArray, MArray (newArray), newListArray, writeArray, readArray)

newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

newSTListArray :: Ix i => (i, i) -> [e] -> ST s (STArray s i e)
newSTListArray = newListArray

executeOps :: Int -> [(Int, Int, Int)] -> [Int]
executeOps n opList = runST $ do
  parent <- newSTListArray (0, n) [0..]
  res <- forM opList (\(op, a, b) -> do
    if op == 0  then do
                  unionParent parent a b
                  return $ -1
                else do
                  pa <- findParent parent a
                  pb <- findParent parent b
                  if pa == pb then return 1
                              else return 0)
  return $ filter (\x -> x /= -1) res

findParent :: STArray s Int Int -> Int -> ST s Int
findParent parent i = do
  dirParent <- readArray parent i
  if dirParent == i
    then return i
    else do 
      newParent <- findParent parent dirParent
      writeArray parent i newParent
      return newParent

unionParent :: STArray s Int Int -> Int -> Int -> ST s ()
unionParent parent i j = do
  pi <- findParent parent i
  pj <- findParent parent j
  writeArray parent pj pi

main :: IO ()
main = do
  [n, m] <- fmap (map read . words) getLine :: IO [Int]

  opList <- replicateM m $ do
    [op, a, b] <- fmap (map read . words) getLine :: IO [Int]
    return (op, a, b)

  let res = executeOps n opList
  putStrLn $ unwords $ map (\x -> if x == 0 then "No" else "Yes") res
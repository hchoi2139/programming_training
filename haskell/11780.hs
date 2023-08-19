import Control.Monad (replicateM, when, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array (Ix (range), Array, array, (!))
import Data.Array.ST (STArray, MArray (newArray), freeze, readArray, writeArray)

type Arr2D = Array (Int, Int) Int

maxVal :: Int
maxVal = 100000 * 100000 + 1

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

floydWarshall :: Int -> [(Int, Int, Int)] -> (Arr2D, Arr2D)
floydWarshall n adjList = runST $ do
  adjArr <- newSTArray ((0, 0), (n, n)) maxVal
  viaArr <- newSTArray ((0, 0), (n, n)) 0

  let initAdj [] = return 0
      initAdj ((a, b, c) : xs) = do
        c' <- readArray adjArr (a, b)
        writeArray adjArr (a, b) $ min c c'
        initAdj xs
  initAdj adjList

  mapM_ (\i -> writeArray adjArr (i, i) 0) [0..n]

  let posPairs = [(i, j) | i <- [1..n], j <- [1..n]]
  forM_ [1..n] (\k -> do
    forM_ posPairs (\(i, j) -> do
      ij <- readArray adjArr (i, j)
      ik <- readArray adjArr (i, k)
      kj <- readArray adjArr (k, j)
      when (ik + kj < ij) $ do writeArray adjArr (i, j) (ik + kj)
                               writeArray viaArr (i, j) k ))

  adjArr' <- freeze adjArr
  viaArr' <- freeze viaArr
  return (adjArr', viaArr')

(+-+) :: [a] -> [a] -> [a]
[] +-+ ys = ys
[x] +-+ ys = ys
(x:xs) +-+ ys = x : (xs +-+ ys)

main :: IO ()
main = do
  n <- readLn :: IO Int
  m <- readLn :: IO Int

  adjList <- replicateM m $ do
    line <- getLine
    let [a, b, c] = fmap read (words line) :: [Int]
    return (a, b, c)

  let (adjArr, viaArr) = floydWarshall n adjList

  let posPairs = [(i, j) | i <- [1..n], j <- [1..n]]
  forM_ posPairs (\(i, j) -> do
    let e = adjArr ! (i, j)
    if e == maxVal then putStr $ show 0 ++ " "
                   else putStr $ show e ++ " "
    when (j == n) $ do putStrLn "")

  forM_ posPairs (\(i, j) -> do
    let e = adjArr ! (i, j)
    if e == 0 || e == maxVal
      then print 0
      else do
        let findRoute s e
              | v == 0 = [s, e]
              | otherwise = findRoute s v +-+ findRoute v e
                where
                  v = viaArr ! (s, e)
        let route = findRoute i j
        putStr $ show (length route) ++ " "
        putStrLn $ unwords $ map show route)
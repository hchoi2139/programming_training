import Control.Monad (replicateM)
import Control.Monad.ST ( ST, runST )
import Data.Array.ST (STArray, MArray (newArray), readArray, writeArray)
import Data.Array (Array, listArray, (!))

data Coord = Coord {
  r :: Int,
  c :: Int
}

data Solver s = Solver {
  n :: Int,
  w :: Int,
  dp :: STArray s (Int, Int) Int,
  locs :: Array Int Coord
}

l1Dist :: Coord -> Coord -> Int
l1Dist c1 c2 = abs (r c1 - r c2) + abs (c c1 - c c2)

newSolver :: [Coord] -> Int -> Int -> ST s (Solver s)
newSolver locs n w = do
  dpArr <- newArray ((0, 0), (w, w)) (-1)
  let locsArr = listArray (0, w) (Coord 0 0 : locs)
  return $ Solver n w dpArr locsArr

findMinDist :: Solver s -> Int -> Int -> ST s Int
findMinDist solver p1 p2
  | p1 == w solver || p2 == w solver = return 0
  | otherwise = do
      minDist <- readArray (dp solver) (p1, p2)
      if minDist /= -1
        then return minDist
        else do
          let cur  = max p1 p2
              next = cur + 1

          dist1 <- if p1 == 0
                   then return $ l1Dist (Coord 1 1) (locs solver ! next)
                   else return $ l1Dist (locs solver ! p1) (locs solver ! next)
          rest1 <- findMinDist solver next p2

          dist2 <- if p2 == 0
                   then return $ l1Dist (Coord (n solver) (n solver)) (locs solver ! next)
                   else return $ l1Dist (locs solver ! p2) (locs solver ! next)
          rest2 <- findMinDist solver p1 next

          let res = min (dist1 + rest1) (dist2 + rest2)
          writeArray (dp solver) (p1, p2) res
          return res

findTrace :: Solver s -> Int -> Int -> ST s [Int]
findTrace solver p1 p2
  | p1 == w solver || p2 == w solver = return []
  | otherwise = do
      let cur  = max p1 p2
          next = cur + 1

      dist1 <- if p1 == 0
               then return $ l1Dist (Coord 1 1) (locs solver ! next)
               else return $ l1Dist (locs solver ! p1) (locs solver ! next)
      rest1 <- readArray (dp solver) (next, p2)

      dist2 <- if p2 == 0
               then return $ l1Dist (Coord (n solver) (n solver)) (locs solver ! next)
               else return $ l1Dist (locs solver ! p2) (locs solver ! next)
      rest2 <- readArray (dp solver) (p1, next)

      let p1Dist = dist1 + rest1
          p2Dist = dist2 + rest2

      remTrace <- if p1Dist < p2Dist
                  then findTrace solver next p2
                  else findTrace solver p1 next
      
      return $ (if p1Dist < p2Dist then 1 else 2) : remTrace

main :: IO ()
main = do
  n <- readLn :: IO Int
  w <- readLn :: IO Int

  locs <- replicateM w $ do
    [r, c] <- fmap (map read . words) getLine :: IO [Int]
    return $ Coord r c
  
  let (minDist, route) = runST $ do
        solver <- newSolver locs n w
        minDist <- findMinDist solver 0 0
        route <- findTrace solver 0 0
        return (minDist, route)
  
  print minDist
  mapM_ print route
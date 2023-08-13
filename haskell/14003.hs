module Main (main, lis) where

import Control.Monad.ST  ( ST, runST )
import Control.Monad     ( (>>=), (=<<), foldM )
import Data.Array.ST     ( Ix,  STArray, readArray, writeArray, newArray )
import Data.Array.MArray ( MArray )

lis :: Ord a => [a] -> [a]
lis xs = runST $ do
  let lxs = length xs
  pileTops  <- newSTArray (min 1 lxs, lxs) []
  i         <- foldM (stack pileTops) 0 xs
  readArray pileTops i >>= return . reverse

stack :: (Integral i, Ord e, Ix i, MArray a [e] m, MonadFail m)
      => a i [e] -> i -> e -> m i
stack piles i x = do
  j <- bsearch piles x i
  writeArray piles j . (x:) =<< if j == 1 then return [] 
                                          else readArray piles (j - 1)
  return $ if j == i + 1 then i + 1 else i

bsearch :: (Integral i, Ord e, Ix i, MArray a [e] m, MonadFail m)
        => a i [e] -> e -> i -> m i
bsearch piles x = go 1
  where
    go lo hi
      | lo > hi   = return lo
      | otherwise = do
          (y:_) <- readArray piles mid
          if y < x then go (succ mid) hi
                   else go lo (pred mid)
            where mid = (lo + hi) `div` 2

newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

main :: IO ()
main = do
  n <- readLn :: IO Int
  input <- getLine
  
  let nums  = map read $ words input :: [Int]
      ans   = lis nums
  
  print $ length ans
  putStrLn $ unwords $ map show ans
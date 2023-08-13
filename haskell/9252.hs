module Main ( main ) where

import Data.Array ( Ix (range), Array, array, listArray, (!) )

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

lcs :: String -> String -> String
lcs xs ys = reverse $ findLCS m n []
  where
    dp :: Array (Int, Int) Int
    dp = tabulate ((0, 0), (m, n)) (uncurry memo)

    memo :: Int -> Int -> Int
    memo 0 _ = 0
    memo _ 0 = 0
    memo i j
      | x == y    = dp ! (i-1, j-1) + 1
      | otherwise = max (dp ! (i-1, j)) (dp ! (i, j-1))
      where
        (x, y) = (axs ! (i-1), ays ! (j-1))

    m = length xs
    n = length ys

    axs, ays :: Array Int Char
    axs = fromList xs
    ays = fromList ys

    findLCS :: Int -> Int -> String -> String
    findLCS i j acc
      | c == 0    = []
      | c == l    = findLCS (i-1) j acc
      | c == t    = findLCS i (j-1) acc
      | otherwise = (axs ! (i-1)) : findLCS (i-1) (j-1) acc
      where
        c = dp ! (i, j)
        l = dp ! (i-1, j)
        t = dp ! (i, j-1)

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  let ans = lcs s1 s2
  print $ length ans
  putStrLn ans
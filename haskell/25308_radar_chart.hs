import Data.Array (Array, listArray, (!))
import Data.List (permutations)

fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

isConvex :: [Int] -> Bool
isConvex polyList = isConvex' 0
  where
    polyArr = fromList polyList

    isConvex' :: Int -> Bool
    isConvex' 8 = True
    isConvex' i = do
      let (ia, ib, ic) = (i, (i + 1) `mod` 8, (i + 2) `mod` 8)
          (a, b, c) = (polyArr ! ia, polyArr ! ib, polyArr ! ic)
      sqrt 2 * fromIntegral (a * c) <= fromIntegral (b * (a + c)) && isConvex' (i + 1)

main :: IO ()
main = do
  input <- getLine
  let stats = read <$> words input :: [Int]
      ans = length $ filter isConvex $ permutations stats
  print ans
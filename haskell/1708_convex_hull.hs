import Control.Monad (replicateM, forM_)
import Data.List (sortBy)

data Point = Point Int Int
  deriving Show

xOf :: Point -> Int
xOf (Point x _) = x

yOf :: Point -> Int
yOf (Point _ y) = y

ccw :: Point -> Point -> Point -> Int
ccw p1 p2 p3 = cp
  where
    a = (xOf p2 - xOf p1, yOf p2 - yOf p1)
    b = (xOf p3 - xOf p1, yOf p3 - yOf p1)
    cp = fst a * snd b - snd a * fst b

cmpYX :: Point -> Point -> Ordering
cmpYX this other
  | yOf this == yOf other = compare (xOf this) (xOf other)
  | otherwise = compare (yOf this) (yOf other)

cmpCCW :: Point -> Point -> Point -> Ordering
cmpCCW mark this other = cmpCCW' $ compare 0 (ccw mark this other)
  where
    cmpCCW' :: Ordering -> Ordering
    cmpCCW' ord
      | ord == EQ = cmpYX this other
      | otherwise = ord

grahamScan :: [Point] -> [Point]
grahamScan []   = []
grahamScan [p]  = [p]
grahamScan [p, p'] = [p, p']
grahamScan rawPs = grahamScan' ps hull
  where
    (mark : ps') = sortBy cmpYX rawPs
    (p1 : p2 : ps) = sortBy (cmpCCW mark) ps'
    hull = [p2, p1]

grahamScan' :: [Point] -> [Point] -> [Point]
grahamScan' [] hull = hull
grahamScan' (cur : rems) hull = grahamScan' rems $ adaptHull hull
  where
    adaptHull :: [Point] -> [Point]
    adaptHull []  = []
    adaptHull [p] = cur : [p]
    adaptHull hull@(p' : p : ps)
      = if ccw p p' cur > 0 then (cur : hull) else adaptHull (p : ps)

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- replicateM n $ do
    line <- getLine
    let (x : y : _) = read <$> words line :: [Int]
    return $ Point x y

  let hull = grahamScan ps
  print $ length hull

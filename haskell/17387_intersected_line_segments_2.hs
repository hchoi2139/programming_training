import Control.Monad (replicateM)

data Point = Point Int Int

instance Eq Point where
  (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2

instance Ord Point where
  (Point x1 y1) <= (Point x2 y2) = if x1 == x2 then y1 <= y2 else x1 <= x2

data Line = Line Point Point

xOf :: Point -> Int
xOf (Point x _) = x

yOf :: Point -> Int
yOf (Point _ y) = y

fstPoint :: Line -> Point
fstPoint (Line p _) = p

sndPoint :: Line -> Point
sndPoint (Line _ p) = p

ccw :: Point -> Point -> Point -> Int
ccw p1 p2 p3
  | cp == 0 = 0
  | cp > 0 = 1
  | otherwise = -1
  where
    a = (xOf p2 - xOf p1, yOf p2 - yOf p1)
    b = (xOf p3 - xOf p1, yOf p3 - yOf p1)
    cp = fst a * snd b - snd a * fst b

main :: IO ()
main = do
  list@(l1 : l2 : _) <- replicateM 2 $ do
    input <- getLine
    let (p : q : r : s : _) = read <$> words input :: [Int]
        p1 = Point p q
        p2 = Point r s
    return $ Line (min p1 p2) (max p1 p2)

  let onL1 = ccw (fstPoint l1) (sndPoint l1) (fstPoint l2) * ccw (fstPoint l1) (sndPoint l1) (sndPoint l2)
      onL2 = ccw (fstPoint l2) (sndPoint l2) (fstPoint l1) * ccw (fstPoint l2) (sndPoint l2) (sndPoint l1)

  if onL1 <= 0 && onL2 <= 0
    then 
      if onL1 == 0 && onL2 == 0 
        then 
          if fstPoint l1 <= sndPoint l2 && fstPoint l2 <= sndPoint l1 
            then print 1 
            else print 0
        else print 1
    else print 0

import Control.Monad (replicateM)

data Point = Point Int Int

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
    let cs@(p : q : r : s : _) = read <$> words input :: [Int]
    return $ Line (Point p q) (Point r s)
  let abc = ccw (fstPoint l1) (sndPoint l1) (fstPoint l2)
      abd = ccw (fstPoint l1) (sndPoint l1) (sndPoint l2)
      cda = ccw (fstPoint l2) (sndPoint l2) (fstPoint l1)
      cdb = ccw (fstPoint l2) (sndPoint l2) (sndPoint l1)
  print $ if abc * abd <= 0 && cda * cdb <= 0 then 1 else 0

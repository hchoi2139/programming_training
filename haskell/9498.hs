main :: IO ()
main = do
  input <- getLine
  let score = read input :: Int
  putChar $ convertToGrade score

convertToGrade :: Int -> Char
convertToGrade score
  | score >= 90 = 'A'
  | score >= 80 = 'B'
  | score >= 70 = 'C'
  | score >= 60 = 'D'
  | otherwise   = 'F'
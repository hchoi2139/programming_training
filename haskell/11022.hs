import Control.Monad (replicateM)

main :: IO ()
main = do
  t <- readLn :: IO Int
  inputs <- replicateM t $ do
    line <- getLine
    let [a, b] = read <$> words line
    return (a, b)
  mapM_ (uncurry printFormat) (zip [1..] inputs)

printFormat :: Int -> (Int, Int) -> IO ()
printFormat idx (a, b) = putStrLn $ 
  "Case #" ++ show idx ++ ": " ++ show a ++ " + " ++ show b ++ " = " ++ show (a + b)
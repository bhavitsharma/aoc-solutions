main :: IO ()
main = do
  contents <- readFile "src/input-1.txt"
  print . solve . map readInt . words $ contents
  print . solve2 . map readInt . words $ contents

readInt :: String -> Int
readInt = read

solve :: [Int] -> Int
solve [] = 0
solve [_] = 0
solve (x : y : xs)
  | x < y = 1 + solve (y : xs)
  | otherwise = solve (y : xs)

rollingSum :: Int -> Int -> Int
rollingSum a b = b - a

slide :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
slide f m xs = zipWith f xs (drop m xs)

slidingWindow :: Int -> [Int] -> [Int]
slidingWindow m xs = scanl (+) start (slide rollingSum m xs)
  where
    start = sum (take m xs)

solve2 :: [Int] -> Int
solve2 = solve . slidingWindow 3
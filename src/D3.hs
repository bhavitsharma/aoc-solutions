import Data.List (transpose)

main :: IO ()
main = do
  contents <- readFile "src/input-3.txt"
  print . solve . words $ contents
  print . solve2 . words $ contents

strToBinary :: [Int] -> Int
strToBinary = foldl (\a b -> a * 2 + b) 0

maxval :: [(Int, Int)] -> [Int]
maxval = map (\(a, b) -> if a > b then 0 else 1)

minval :: [(Int, Int)] -> [Int]
minval = map (\(a, b) -> if a > b then 1 else 0)

solve :: [[Char]] -> Int
solve x = gamma * epislon
  where
    t = transpose x
    gamma = strToBinary . maxval $ allVals
    epislon = strToBinary . minval $ allVals
    allVals = fmap zeroOneCount t

zeroOneCount :: [Char] -> (Int, Int)
zeroOneCount =
  foldl
    ( \(a, b) c ->
        if c == '1'
          then (a, b + 1)
          else (a + 1, b)
    )
    (0, 0)

-- All individual char list must have same length
recurseMax :: [[Char]] -> Int -> (Int -> Int -> Char) -> [Int]
recurseMax x k comp
  | k == len || length x == 1 = map (\c -> if c == '1' then 1 else 0) (head x)
  | otherwise = recurseMax xs (k + 1) comp
  where
    t = transpose x
    (a, b) = zeroOneCount (t !! k)
    bit = comp a b
    xs = filter (\str -> str !! k == bit) x
    len = length . head $ x

solve2 :: [[Char]] -> Int
solve2 x = oxygen * carbon
  where
    oxygen = strToBinary . recurseMax x 0 $ (\a b -> if a <= b then '1' else '0')
    carbon = strToBinary . recurseMax x 0 $ (\a b -> if a <= b then '0' else '1')
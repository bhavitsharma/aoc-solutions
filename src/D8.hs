import Data.Char (ord)
import Data.List (find, group, permutations, sort)

segs = [([1, 2, 3, 5, 6, 7], 0), ([3, 6], 1), ([1, 3, 4, 5, 7], 2), ([1, 3, 4, 6, 7], 3), ([2, 3, 4, 6], 4), ([1, 2, 4, 6, 7], 5), ([1, 2, 4, 5, 6, 7], 6), ([1, 3, 6], 7), ([1, 2, 3, 4, 5, 6, 7], 8), ([1, 2, 3, 4, 6, 7], 9)]

fstsegs = fmap fst segs

perm = permutations [1, 2, 3, 4, 5, 6, 7]

replace :: Char -> Int
replace a = 1 + ord a - ord 'a'

try :: [[Int]] -> [Int] -> Bool
try l r = all (`elem` fstsegs) m
  where
    m = fmap (sort . fmap (\i -> r !! (i - 1))) l

findNum :: [Int] -> Int
findNum l = maybe (-1) snd . find (\(a, _) -> a == l) $ segs

solve :: ([[Char]], [[Char]]) -> [Int]
solve (l, r) = ans
  where
    digits = fmap (sort . map replace) l
    nums = fmap (sort . map replace) r
    (Just f) = find (try digits) perm
    ans = fmap (findNum . sort . fmap (\i -> f !! (i - 1))) nums

parseT :: [[Char]] -> ([[Char]], [[Char]])
parseT = splitAt 10 . filter ("|" /=)

arr = [1, 4, 7, 8]

count :: [[Int]] -> Int
count = sum . map (length . filter (`elem` arr))

main :: IO ()
main = do
  contents <- readFile "src/input-8.txt"
  print . sum . map (read . concatMap show . solve . parseT . words) . lines $ contents
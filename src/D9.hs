import Data.Char (digitToInt)
import Data.List (nub, sort)
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Int)

main :: IO ()
main = do
  contents <- readFile "src/input-9.txt"
  print . solve . V.fromList . fmap parseCustom . lines $ contents

parseCustom :: String -> V.Vector Int
parseCustom = V.fromList . fmap digitToInt

dir = [(-1, 0), (0, 1), (0, -1), (1, 0)]

atIndex :: V.Vector (V.Vector Int) -> Int -> Int -> Int
atIndex v x y = (v V.! x) V.! y

type Vis = [(Int, Int)]

neighbors :: Grid -> Int -> Int -> [(Int, (Int, Int))]
neighbors v x y = newDirs
  where
    n = V.length v
    m = V.length (v V.! 0)
    newDirs = fmap (\(a, b) -> (atIndex v a b, (a, b))) . filter isInside . fmap (\(a, b) -> (x + a, y + b)) $ dir
    isInside (a, b) = a >= 0 && a < n && b >= 0 && b < m

dfs :: Grid -> Vis -> Int -> Int -> Vis
dfs g vis x y = ndfs
  where
    val = atIndex g x y
    neig = filter (\(e, node) -> e /= 9 && e > val && node `notElem` vis) (neighbors g x y)
    ndfs = foldl (\a (_, (i, j)) -> nub (a ++ dfs g a i j)) (vis ++ [(x, y)]) neig

check :: Grid -> Int -> Int -> Int
check v x y
  | val < neig = length (dfs v [] x y)
  | otherwise = 0
  where
    neigh = neighbors v x y
    val = atIndex v x y
    neig = foldl (\a b -> min a (fst b)) 10000 neigh

solve v = val
  where
    n = V.length v
    m = V.length (v V.! 0)
    val = product . take 3 . reverse . sort $ [check v x y | x <- [0 .. (n -1)], y <- [0 .. (m - 1)]]

import Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Int)

type NMap = M.Map (Int, Int) Int

type Vis = S.Set (Int, Int)

dir :: [(Int, Int)]
dir = [(-1, 0), (0, 1), (0, -1), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1)]

atIndex :: V.Vector (V.Vector Int) -> Int -> Int -> Int
atIndex v x y = (v V.! x) V.! y

neighbors :: Grid -> Int -> Int -> [(Int, (Int, Int))]
neighbors v x y = newDirs
  where
    (n, m) = (10, 10)
    newDirs = fmap (\(a, b) -> (atIndex v a b, (a, b))) . filter isInside . fmap (\(a, b) -> (x + a, y + b)) $ dir
    isInside (a, b) = a >= 0 && a < n && b >= 0 && b < m

dfsAcc :: Grid -> (Vis, NMap) -> (Int, Int) -> (Vis, NMap)
dfsAcc g (vis, nmap) (x, y) = (vis', nmap'')
  where
    nmap' = M.insertWith (\_ b -> b + 1) (x, y) 1 nmap
    (vis', nmap'') = dfs g (x, y) (vis, nmap')

dfs :: Grid -> (Int, Int) -> (Vis, NMap) -> (Vis, NMap)
dfs g (x, y) (visited, inc)
  | val < 10 = (visited, inc)
  | S.member (x, y) visited = (visited, inc)
  | otherwise = f
  where
    val = atIndex g x y + M.findWithDefault 0 (x, y) inc
    neig = fmap snd (neighbors g x y)
    f = foldl (dfsAcc g) (S.insert (x, y) visited, inc) neig

combinations = [(x, y) | x <- [0 .. 9], y <- [0 .. 9]]

mapF :: (Int, Int) -> Int -> NMap -> Int
mapF p v m = if val > 9 then 0 else val
  where
    val = v + M.findWithDefault 0 p m

step :: Grid -> (Int, Grid)
step g = (S.size visited, nGrid)
  where
    incG = fmap (fmap (+ 1)) g
    (visited, trans) = foldl (flip (dfs incG)) (S.empty, M.empty) combinations
    nGrid = V.imap (\idx a -> V.imap (\idy b -> mapF (idx, idy) b trans) a) incG

recurseN :: Int -> Grid -> Int
recurseN n g
  | allZ = n
  | otherwise = recurseN (n + 1) g'
  where
    (_, g') = step g
    allZ = allZero g'

allZero :: Grid -> Bool
allZero = V.all (V.all (== 0))

main :: IO ()
main = do
  contents <- readFile "src/input-11.txt"
  print . recurseN 1 . V.fromList . fmap parseCustom . lines $ contents

parseCustom :: String -> V.Vector Int
parseCustom = V.fromList . fmap digitToInt
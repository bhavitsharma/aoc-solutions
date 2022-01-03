import Data.Char (digitToInt)
import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Vector as V

type Grid = [[Int]]

type VGrid = V.Vector (V.Vector Int)

type Edge = (Int, Int) -- to, weight

type Graph = M.Map Int [Edge]

type DistanceVector = M.Map Int Int

type DState = (H.MinHeap (Int, Int), DistanceVector)

type DOutput = DistanceVector

nodeIndex :: Int -> Int -> VGrid -> Int
nodeIndex x y grid = m * x + y
  where
    m = V.length (V.head grid)

dir :: [(Int, Int)]
dir = [(0, 1), (1, 0), (-1, 0), (0, -1)]

inside :: V.Vector (V.Vector a) -> (Int, Int) -> Bool
inside grid (a, b) = a >= 0 && b >= 0 && a < n && b < m
  where
    n = V.length grid
    m = V.length (V.head grid)

acc :: Int -> Int -> Int -> VGrid -> [(Int, [Edge])]
acc x y val grid = [(node, neighbors)]
  where
    node = nodeIndex x y grid
    neighbors = fmap (\(a, b) -> (nodeIndex a b grid, val)) . filter (inside grid) . fmap (\(a, b) -> (x + a, y + b)) $ dir

makeGraph :: Grid -> Graph
makeGraph g = M.fromList graph
  where
    vG = V.fromList . fmap V.fromList $ g
    graph = V.ifoldr' (\x value lst -> lst ++ V.ifoldr' (\y val l -> l ++ acc x y val vG) [] value) [] vG

-- dijkstra :: Int -> Graph -> [Int]
-- dijkstra start graph =

main :: IO ()
main = do
  contents <- readFile "src/input-15.txt"
  print . makeGraph . fmap (fmap digitToInt) . lines $ contents
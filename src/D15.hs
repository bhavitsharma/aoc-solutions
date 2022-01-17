import Control.Lens (set, _2, _3)
import qualified Control.Monad.Trans.State.Strict as S
import Data.Char (digitToInt)
import qualified Data.Heap as H
import Data.List (transpose)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Vector as V

type Grid = [[Int]]

type VGrid = V.Vector (V.Vector Int)

type Edge = (Int, Int) -- to, weight

type Graph = M.Map Int [Edge]

type DistanceVector = M.Map Int Int

-- (weight, node) == (Int, Int)
type DState = (Graph, H.MinHeap (Int, Int), DistanceVector)

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

canUpdateEdge :: DistanceVector -> (Int, Int) -> Bool
canUpdateEdge dMap (node, distance) = distance < d
  where
    d = fromJust . M.lookup node $ dMap

runDijkstra :: Int -> Int -> Graph -> DistanceVector
runDijkstra start totalNodes g = d
  where
    dVector = M.fromList [(i, 1000000000 :: Int) | i <- [0 .. totalNodes]]
    (_, _, d) = S.execState dijkstra (g, H.insert (0, start) H.empty, dVector)

dijkstra :: S.State DState ()
dijkstra = do
  -- get the edge with lowest value
  (graph, minHeap, distances) <- S.get
  let item = H.view minHeap
  case item of
    Nothing -> return ()
    Just ((d, node), newHeap) -> do
      let currentMin = fromJust . M.lookup node $ distances
      if currentMin <= d
        then S.modify (set _2 newHeap) >> dijkstra
        else do
          -- set value
          let newD = M.adjust (const d) node distances
          case M.lookup node graph of
            Nothing -> S.modify (set _2 newHeap) >> S.modify (set _3 newD) >> dijkstra
            (Just edges) -> do
              let newNodesToInsert = filter (canUpdateEdge newD) . fmap (\(v, weight) -> (v, weight + d)) $ edges
              let newHeap' = foldr (\(u, dist) -> H.insert (dist, u)) newHeap newNodesToInsert
              S.modify (set _2 newHeap')
              S.modify (set _3 newD)
              dijkstra

incAll :: [[Int]] -> Int -> [[Int]]
incAll xs n = [[if r > 9 then r - 9 else r | i <- rs, let r = i + n] | rs <- xs]

multiplyGrid :: [[Int]] -> [[Int]]
multiplyGrid xs = concatMap (incAll megaRow) [0 .. 4]
  where
    megaRow = transpose $ concatMap (incAll (transpose xs)) [0 .. 4]

solve :: Grid -> Int
solve g = adjust + (fromJust . M.lookup (totalNodes - 1) $ distanceVector)
  where
    graph = makeGraph g
    row = length g
    totalNodes = row * row
    distanceVector = runDijkstra 0 (totalNodes - 1) graph
    adjust = g !! (row - 1) !! (row - 1) - g !! 0 !! 0

main :: IO ()
main = do
  contents <- readFile "src/input-15.txt"
  print . solve . multiplyGrid . fmap (fmap digitToInt) . lines $ contents

import Control.Lens (set, view, _1, _2, _3)
import Control.Monad (guard)
import qualified Control.Monad.State.Strict as S
import Data.Array (array)
import Data.Array.Base ((!))
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)

type Edge = (Int, Integer) -- to, weight

type Graph = M.Map Int [Edge]

type DistanceVector = M.Map Int Integer

-- (weight, node) == (Int, Int)
type DState = (Graph, H.MinHeap (Integer, Int), DistanceVector)

-- 0, 1, 2, 3 -> Char
type Position = (Int, Int)

-- Position A, B, C, D
type State = (M.Map Int Int, [[Position]])

type Visited = M.Map State [(State, Int)]

unchanged = array (0, 3) [(0, 2), (1, 4), (2, 6), (3, 8)]

powers = array (0, 3) [(0, 1), (1, 10), (2, 100), (3, 1000)]

leftPos u m = case M.lookupLT u m of
  Nothing -> 0
  Just (x, _) -> x + 1

rightPos u m = case M.lookupGT u m of
  Nothing -> 10
  Just (x, _) -> x - 1

removeTop v index = x ++ [h] ++ y
  where
    h = tail $ v !! index
    x = take index v
    y = drop (index + 1) v

-- TODO(bhavit): The `moveHall` and `moveBin` are really really ugly. Try to refactor when you have time.
moveBin (m, v) index
  | null v' = []
  | otherwise = map (\(i, cost) -> ((M.insert i char m, newL), cost)) position
  where
    v' = v !! index
    (idx, char) = head v'
    newL = removeTop v index
    u = unchanged ! index
    pow = powers ! char
    left = leftPos u m
    right = rightPos u m
    position = [(i, (idx + 1 + abs (i - u)) * pow) | i <- [left .. right], i `notElem` [2, 4, 6, 8]]

moveHall (m, pos) index = [fromJust x | x <- [a, b], isJust x]
  where
    u = unchanged ! index
    v' = pos !! index
    (position, _) = head v'
    newPos@(idx, _) = if null v' then (3, index) else (position - 1, index)
    allSame = all ((==) index . snd) v'
    l = take index pos
    r = drop (index + 1) pos
    newL = l ++ [newPos : v'] ++ r
    pow = powers ! index
    newState f = do
      (k, v) <- f u m
      guard (v == index && allSame)
      let c1 = pow * (idx + 1 + abs (k - u))
      return ((M.delete k m, newL), c1)
    a = newState M.lookupLT
    b = newState M.lookupGT

newStates :: State -> [(State, Int)]
newStates s = s1 ++ s2
  where
    s1 = concatMap (moveBin s) [0 .. 3]
    s2 = concatMap (moveHall s) [0 .. 3]

createGraph :: State -> S.State Visited ()
createGraph s = do
  visited <- S.get
  if M.member s visited
    then return ()
    else do
      let edges = newStates s
      S.modify (M.insert s edges)
      mapM_ (createGraph . view _1) edges

-- canUpdateEdge :: DistanceVector -> (Int, Int) -> Bool
canUpdateEdge dMap (node, distance) = distance < d
  where
    d = fromJust . M.lookup node $ dMap

runDijkstra :: Int -> Int -> Graph -> DistanceVector
runDijkstra start totalNodes g = d
  where
    dVector = M.fromList [(i, 1000000000 :: Integer) | i <- [0 .. totalNodes]]
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

runDijkstra' start = M.lookup endIndex dVector
  where
    totalNodes = M.size graph
    graph = S.execState (createGraph (M.empty, start)) M.empty
    newMap = M.map (fmap (\(vertex, c) -> (M.findIndex vertex graph, toInteger c))) . M.mapKeys (`M.findIndex` graph) $ graph
    startIndex = M.findIndex (M.empty, start) graph
    end = [[(j, i) | j <- [0 .. 3]] | i <- [0 .. 3]]
    endIndex = M.findIndex (M.empty, end) graph
    dVector = runDijkstra startIndex totalNodes newMap

main :: IO ()
main = do
  let v =
        [ [(0, 2), (1, 3), (2, 3), (3, 1)],
          [(0, 0), (1, 2), (2, 1), (3, 0)],
          [(0, 1), (1, 1), (2, 0), (3, 3)],
          [(0, 3), (1, 0), (2, 2), (3, 2)]
        ]
  print . runDijkstra' $ v
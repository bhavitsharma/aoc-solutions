import qualified Data.Vector as V
import Data.List (find)
import Data.Function (on)
import Control.Lens hiding (index)
import qualified Data.Vector.Algorithms.Merge as V
import Control.Monad.ST as ST
import qualified Data.Set as S

type Point = (Int, Int, Bool)
type Event = (EventType, Int, Int) -- type, coordinate, index (in original array)
data EventType = Start | End deriving (Enum, Show, Eq)

getEvents :: Int -> Point -> V.Vector Event
getEvents index (x1, x2, _) = V.fromList [(Start, x1, index), (End, x2, index)]

type Coordinate = Int
type Segs = S.Set Int
type Intervals = [(Int, Int, Int)]
type EState = (Int, Coordinate, Segs, Intervals)

isInside :: V.Vector Point -> Int -> Int -> Bool
isInside v p index = x1 <= p && p <= x2
  where
    (x1, x2, _) = v V.! index

getLargestIndex :: Segs -> V.Vector Point -> Int -> Maybe Int
getLargestIndex segs points point = find (isInside points point) s
  where
    s = S.toDescList segs

f :: Bool -> Int
f True = 1
f False = 0

getPoint :: Segs -> V.Vector Point -> Int -> Int
getPoint segs v start = case fd of
      Nothing -> 0
      Just idx -> f lightState
        where
          (_, _, lightState) = v V.! idx
  where
    fd = getLargestIndex segs v start

adjustArea :: EState -> Event -> V.Vector Point -> EState
adjustArea (st, x1, segs, l) (eType, x2, index) v = newState
  where
    newSegs = if eType == Start then S.insert index segs else S.delete index segs
    end = if eType == Start then x2 - 1 else x2
    start = if eType == Start then x2 else x2 + 1
    newL = (x1, end, st):l
    newState = (getPoint newSegs v start, start, newSegs, newL)

inf :: Int
inf = -100000000

shouldConsider (x1, x2, st) = st > 0 && x1 <= x2

lineArea points = ans -- foldr (\(x1, x2, area) acc -> acc + (x2 - x1 + 1) * area) 0 . filter shouldConsider $ ans -- sortBy (compare `on` view _1) ans
  where
    events = sortVec . V.concat . V.ifoldr (\index p acc -> acc ++ [getEvents index p]) [] $ points
    startState = (0, inf, S.empty, [])
    (_, _, _, ans) = V.foldl' (\estate event -> adjustArea estate event points) startState events

-- type Rect = ((Int, Int), (Int, Int), Light)
-- 
-- getEvents2 :: Int -> Rect -> V.Vector Event
-- getEvents2 index (x1, x2, _) = V.fromList [(Start, view _1 x1, index), (End, view _1 x2, index)]
-- rectangleArea :: V.Vector Rect -> Int
-- rectangleArea points = error "not implemented"
--   where
--     events = sortVec . V.concat . V.ifoldr (\index p acc -> acc ++ [getEvents2 index p]) [] $ points
--     
-- 

sortVec :: V.Vector Event -> V.Vector Event
sortVec vec = ST.runST $ do
    mvec <- V.thaw vec
    V.sortBy (compare `on` view _2) mvec
    V.freeze mvec

import Control.Lens hiding (index)
import Control.Monad (foldM)
import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V hiding (foldr')
import Text.Parsec (Parsec, char, digit, endBy1, letter, many1, newline, parse, sepBy, string, try, (<|>))

type Point = (Int, Int, Int)

type Cube = (Point, Point, Bool)

type Event = (EventType, Int, Int) -- type, coordinate, index (in original array)

data EventType = Start | End deriving (Enum, Show, Eq)

type Coordinate = Int

type Segs = S.Set Int

type Intervals = [(Int, Int, Integer)]

type EState = (Integer, Coordinate, Segs, Intervals)

data Axis = X | Y | Z deriving (Enum, Show, Eq)

-- Sweep Line Algorithms generalized to 3 dimensions.
-- Runs the final input in under 6 seconds.
-- The idea is this: Instead of line, we're sweeping a const `X` plane and then calculating the vol.
-- The worst case complexity is O(n^4), but in reality it's a bit.
-- A lot of data structure can be optimized. For example `getLargestIndex` can be optimized a lot.
-- Really need to learn state monads now SMH.
generateEvents :: (Point -> Int) -> V.Vector Cube -> M.Map Int [Event]
generateEvents f points = v'
  where
    accum acc index (p1, p2, _) = (f p1, [(Start, f p1, index)]) : (f p2, [(End, f p2, index)]) : acc
    v' = M.fromListWith (++) . V.ifoldl' accum [] $ points

generateX :: V.Vector Cube -> M.Map Int [Event]
generateX = generateEvents (view _1)

generateY :: V.Vector Cube -> M.Map Int [Event]
generateY = generateEvents (view _2)

generateZ :: V.Vector Cube -> M.Map Int [Event]
generateZ = generateEvents (view _3)

isInside :: Int -> Cube -> Bool
isInside p ((_, _, x1), (_, _, x2), _) = x1 <= p && p <= x2

getPoint :: Int -> V.Vector Cube -> Integer
getPoint start v = case fd of
  Nothing -> 0
  Just (_, _, lightState) -> toInteger . fromEnum $ lightState
  where
    fd = V.find (isInside start) . V.reverse $ v

inf :: Int
inf = -100000000

zero :: Integer
zero = 0

-- get<Func> can be refactored to use State monad and more generalizations
getLowerDimensionalVol :: Int -> V.Vector Cube -> Axis -> Integer
getLowerDimensionalVol x v axis = case axis of
  X -> getVol v Y
  Y -> getVol v Z
  Z -> getPoint x v

inCube :: S.Set Int -> V.Vector a -> V.Vector a
inCube s = V.ifilter (\index _ -> S.member index s)

adjustVol :: EState -> [Event] -> V.Vector Cube -> Axis -> EState
adjustVol (st, a, segs, l) events points axis = (nXArea, x + 1, allSegs, newL)
  where
    x = view _2 (head events)
    startSegs = S.union segs . S.fromList . fmap (view _3) . filter (\e -> view _1 e == Start) $ events
    allSegs = S.difference startSegs . S.fromList . fmap (view _3) . filter (\e -> view _1 e == End) $ events
    xArea = getLowerDimensionalVol x (inCube startSegs points) axis
    nXArea = getLowerDimensionalVol (x + 1) (inCube allSegs points) axis
    newL = (a, x - 1, st) : (x, x, xArea) : l

getVol :: V.Vector Cube -> Axis -> Integer
getVol points axis = a
  where
    events = case axis of
      X -> generateX points
      Y -> generateY points
      Z -> generateZ points
    startState = (zero, inf, S.empty, [])
    l = view _4 . M.foldl' (\st e -> adjustVol st e points axis) startState $ events
    a = foldr (\(x1, x2, area) acc -> acc + toInteger (x2 - x1 + 1) * area) zero . filter (\(x1, x2, _) -> x1 <= x2) $ l

-- type MyState = (V.Vector Cube, Axis)
-- experimental monad implementation
-- getPoint2 :: Int -> St.State MyState Integer
-- getPoint2 start = do
--   fd <- St.gets (V.find (isInside start) . V.reverse . view _1)
--   return $ case fd of
--     Nothing -> 0
--     Just (_, _, lightState) -> toInteger . fromEnum $ lightState

-- getLowerDimensionalVol2 :: Int -> St.State MyState Integer
-- getLowerDimensionalVol2 x = do
--   axis <- St.gets (view _2)
--   case axis of
--     X -> St.modify (set _2 Y) >> getVol2
--     Y -> St.modify (set _2 Z) >> getVol2
--     Z -> getPoint2 x

-- adjustVol2 :: [Event] -> EState -> St.State MyState EState
-- adjustVol2 events (st, a, segs, l) = do
--   let x = view _2 (head events)
--   let startSegs = S.union segs . S.fromList . fmap (view _3) . filter (\e -> view _1 e == Start) $ events
--   let allSegs = S.difference startSegs . S.fromList . fmap (view _3) . filter (\e -> view _1 e == End) $ events
--   (points, axis) <- St.get
--   St.modify (set _1 (inCube startSegs points))
--   xArea <- getLowerDimensionalVol2 x

--   St.modify (set _1 (inCube allSegs points))
--   St.modify (set _2 axis)
--   nXArea <- getLowerDimensionalVol2 (x + 1)
--   let newL = (a, x - 1, st) : (x, x, xArea) : l
--   return (nXArea, x + 1, allSegs, newL)

-- getEventWithAxis :: V.Vector Cube -> Axis -> M.Map Int [Event]
-- getEventWithAxis points axis = case axis of
--   X -> generateX points
--   Y -> generateY points
--   Z -> generateZ points

-- getVol2 :: St.State MyState Integer
-- getVol2 = do
--   events <- St.gets $ uncurry getEventWithAxis
--   let startState = (zero, inf, S.empty, [])
--   l <- view _4 <$> foldM (flip adjustVol2) startState events
--   return $ foldr (\(x1, x2, area) acc -> acc + toInteger (x2 - x1 + 1) * area) zero . filter (\(x1, x2, _) -> x1 <= x2) $ l

-- parsing and shit --
main :: IO ()
main = do
  contents <- readFile "src/input-22.txt"
  let (Right cubes) = parse parser "" contents
  let v = V.fromList cubes
  print . getVol v $ X

-- print $ St.evalState getVol2 (v, X)

parser :: Parsec String () [Cube]
parser = parseCube `endBy1` newline

parseCube :: Parsec String () Cube
parseCube = do
  action <- try (string "off") <|> string "on"
  let a = case action of
        "off" -> False
        _ -> True
  _ <- char ' '
  [(x1, x2), (y1, y2), (z1, z2)] <- parsePoint `sepBy` char ','
  return ((x1, y1, z1), (x2, y2, z2), a)

parsePoint :: Parsec String () (Int, Int)
parsePoint = do
  _ <- letter
  _ <- char '='
  n1 <- number
  _ <- string ".."
  n2 <- number
  return (n1, n2)

number :: Parsec String () Int
number = read <$> many1 (digit <|> char '-')

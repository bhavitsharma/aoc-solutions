import qualified Data.Map as M
import Text.Parsec

type Segment = (Int, Int, Int, Int)

main :: IO ()
main = do
  txt <- readFile "src/input-5.txt"
  let (Right segments) = parse parser "" txt
  print . solve $ segments

solve :: [Segment] -> Int
solve s = M.size . M.filter (> 1) $ finalMap
  where
    finalMap = foldl (\acc -> M.unionWith (+) acc . getMap) M.empty s

getMap :: Segment -> M.Map (Int, Int) Int
getMap (x1, y1, x2, y2)
  | dx == 0 = M.fromList [((x1, i), 1) | i <- [(min y1 y2) .. max y1 y2]]
  | dy == 0 = M.fromList [((i, y1), 1) | i <- [(min x1 x2) .. max x1 x2]]
  | abs dx == abs dy = M.fromList [((x2 + i * d1, y2 + i * d2), 1) | i <- [0 .. n]]
  | otherwise = M.empty
  where
    dx = x1 - x2
    dy = y1 - y2
    n = abs dx
    d1 = dx `div` abs dx
    d2 = dy `div` abs dy

parser :: Parsec String () [Segment]
parser =
  ( do
      x1 <- number
      _ <- char ','
      y1 <- number

      _ <- string " -> "

      x2 <- number
      _ <- char ','
      y2 <- number

      return (x1, y1, x2, y2)
  )
    `sepEndBy` newline

number :: Parsec String () Int
number = read <$> many1 digit
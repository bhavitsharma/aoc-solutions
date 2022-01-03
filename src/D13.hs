import Data.List (intercalate, nub)
import Text.Parsec (Parsec, char, digit, many1, newline, parse, sepBy1, sepEndBy1, (<|>))
import Text.Parsec.Char (string)
import Text.Printf (printf)

type Point = (Int, Int)

handleY, handleX :: Int -> [Point] -> [Point]
handleY pivot lst = nub lst''
  where
    lst' = filter (\(x, y) -> y /= pivot) lst
    lst'' = fmap (\(x, y) -> if y < pivot then (x, y) else (x, 2 * pivot - y)) lst'
handleX pivot lst = nub lst''
  where
    lst' = filter (\(x, y) -> x /= pivot) lst
    lst'' = fmap (\(x, y) -> if x < pivot then (x, y) else (2 * pivot - x, y)) lst'

transform :: (Bool, Int) -> [Point] -> [Point]
transform (dir, magnitude)
  | dir = handleY magnitude
  | otherwise = handleX magnitude

maxP :: [Point] -> Point
maxP lst = (x, y)
  where
    x = maximum (fmap fst lst)
    y = maximum (fmap snd lst)

makeGrid :: [Point] -> [String]
makeGrid lst = [[if (i, j) `elem` lst then '#' else '.' | j <- [0 .. y]] | i <- [0 .. x]]
  where
    (x, y) = maxP lst

solve2 :: [(Bool, Int)] -> [Point] -> [Point]
solve2 ins points = foldl (flip transform) points ins

main :: IO ()
main = do
  contents <- readFile "src/input-13.txt"
  let (Right (points, ins)) = parse parser "" contents
  print . makeGrid . solve2 ins $ points

parser :: Parsec String () ([Point], [(Bool, Int)])
parser = do
  coords <-
    ( do
        [x, y] <- number `sepBy1` char ','
        return (x, y)
      )
      `sepEndBy1` newline
  newline
  folds <- foldInstruction `sepEndBy1` newline
  return (coords, folds)

foldInstruction = do
  _ <- string "fold along "
  dir <- string "x=" <|> string "y="
  v <- number

  return $ case dir of
    "y=" -> (True, v)
    _ -> (False, v)

number :: Parsec String () Int
number = read <$> many1 digit
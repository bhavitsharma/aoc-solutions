import Data.List (find, sortOn, transpose)
import Text.Parsec

parser :: Parsec String () ([Int], [Board])
parser = do
  nums <- number `sepBy` char ','
  newline
  newline
  boards <- parseBoard `sepBy1` newline
  return (nums, boards)

number :: Parsec String () Int
number = read <$> many1 digit

parseBoard :: Parsec String () Board
parseBoard = many1 $ do
  try
    ( do
        many spaceChar
        xs <- number `sepEndBy1` many1 spaceChar
        newline
        return xs
    )

spaceChar = char ' '

type Board = [[Int]]

main :: IO ()
main = do
  contents <- readFile "src/input-4.txt"
  let (Right (moves, grids)) = parse parser "" contents
  print (solve moves grids r2 last)
  print (solve moves grids r2 head)

solve moves grids recurseFn listFetcher = (winnerBoard, calledNum, unmarkedSum * calledNum)
  where
    unmarkedSum = sum f
    f = filter (/= -1) (concat winnerBoard)
    (winnerBoard, calledNum) = recurseFn moves grids listFetcher

newBoard :: Int -> Board -> Board
newBoard n = map (map (\v -> if v == n then (-1) else v))

isWon :: Board -> Bool
isWon b = won b || won (transpose b)
  where
    won = any (all (== -1))

r2 :: [Int] -> [Board] -> ([Board] -> Board) -> (Board, Int)
r2 [] _ _ = error "Something went wrong"
r2 (x : xs) b f = if isWon (f transformed) then (f transformed, x) else r2 xs transformed f
  where
    transformed = sortOn isWon (map (newBoard x) b)
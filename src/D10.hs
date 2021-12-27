import Control.Monad (foldM)
import Data.List (sort)

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

points '(' = 1
points '[' = 2
points '{' = 3
points '<' = 4

isOpening :: Char -> Bool
isOpening ')' = False
isOpening ']' = False
isOpening '}' = False
isOpening '>' = False
isOpening _ = True

opp :: Char -> Char
opp ')' = '('
opp '>' = '<'
opp '}' = '{'
opp ']' = '['

addToStack :: [Char] -> Char -> Either Char [Char]
addToStack [] c = if isOpening c then Right [c] else Left c
addToStack (x : xs) c
  | isOpening c = Right (c : x : xs)
  | x == op = Right xs
  | otherwise = Left c
  where
    op = opp c

createStack :: [Char] -> Either Char [Char]
createStack = foldM addToStack []

solve :: [Char] -> Integer
solve c = case createStack c of
  Right xs -> foldl (\a b -> a * 5 + points b) 0 xs
  _ -> 0

findMid :: [Integer] -> Integer
findMid x = (!! max 0 n2) x
  where
    n = length x
    n2 = n `div` 2

main :: IO ()
main = do
  file <- readFile "src/input-10.txt"
  print . findMid . sort . filter (/= 0) . fmap solve . lines $ file
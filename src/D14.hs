import Data.List (sort)
import qualified Data.Map as M
import Text.Parsec (Parsec, char, digit, letter, many1, newline, parse, sepBy1, sepEndBy1, (<|>))
import Text.Parsec.Char (string)

type Frequency = M.Map String Integer

type Mapping = M.Map String Char

acc :: Mapping -> String -> Integer -> [(String, Integer)]
acc m s v = case M.lookup s m of
  Nothing -> []
  Just x -> [(s1, v), (s2, v)]
    where
      s1 = head s : [x]
      s2 = x : [last s]

move :: Frequency -> Mapping -> Frequency
move f m = f'
  where
    lst = M.foldrWithKey (\key value l -> acc m key value ++ l) [] f
    f' = M.fromListWith (+) lst

recurse :: Integer -> Frequency -> Mapping -> Frequency
recurse 1 f m = move f m
recurse n f m = recurse (n - 1) (move f m) m

allSubs :: Int -> String -> [String]
allSubs n s
  | length s >= n = take n s : allSubs n (tail s)
  | otherwise = []

formSubF :: String -> M.Map String Integer
formSubF = M.fromListWith (+) . fmap (\i -> (i, 1)) . allSubs 2

fFromMap :: Frequency -> M.Map Char Integer
fFromMap = M.fromListWith (+) . M.foldrWithKey (\key value acc -> acc ++ fmap (\c -> (c, value)) key) []

findAns :: [Integer] -> Integer
findAns l = last l - head l

main :: IO ()
main = do
  contents <- readFile "src/input-14.txt"
  let (Right (s, m)) = parse parser "" contents
  let f' = formSubF s
  print (findAns . sort . fmap (\x -> (x + 1) `div` 2) . M.elems . fFromMap . recurse 40 f' $ m)

parser :: Parsec String () (String, Mapping)
parser = do
  template <- many1 letter
  newline
  newline
  rules <- rule `sepEndBy1` newline
  return (template, M.fromList rules)

rule = do
  ab <- many1 letter
  string " -> "
  c <- letter
  return (ab, c)
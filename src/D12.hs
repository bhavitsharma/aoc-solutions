import Data.Char (isLower)
import Data.List (elemIndex)
import Data.Map ((!))
import qualified Data.Map as M

type Graph = M.Map String [String]

type VisMap = M.Map String Int

isSmall :: String -> Bool
isSmall = all isLower

-- for part1, replace condition with just `cnt == 0`
canVisit :: String -> VisMap -> Bool
canVisit s m
  | cnt == 0 = True
  | cnt == 1 = 2 `notElem` M.elems m && s /= "start" -- remove this for the part1.
  | otherwise = False
  where
    cnt = M.findWithDefault 0 s m

-- constraints are small enough to just brute force instead of dp.
-- Although implementing DP with haskell is so freakin hard.
-- verified that answer can never be infinite because every "large" node is connected to one small.
dfs :: String -> VisMap -> Graph -> Int
dfs st visited g
  | st == "end" = 1
  | not (canVisit st visited) = 0
  | otherwise = foldl (\acc x -> acc + dfs x vis' g) 0 neighbors
  where
    neighbors = g ! st
    vis' = if isSmall st then M.insertWith (+) st 1 visited else visited

edge :: String -> (String, String)
edge s = (x, drop 1 y)
  where
    (Just idx) = elemIndex '-' s
    (x, y) = splitAt idx s

formGraph :: [String] -> Graph
formGraph lst = M.fromListWith (++) edges
  where
    e = fmap edge lst
    edges = foldl (\acc (x, y) -> acc ++ [(x, [y]), (y, [x])]) [] e

main :: IO ()
main = do
  contents <- readFile "src/input-12.txt"
  print . dfs "start" M.empty . formGraph . lines $ contents
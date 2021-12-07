import qualified Data.Map as M
import Text.Parsec

parser :: Parsec String () [Integer]
parser =
  do
    nums <- number `sepBy` char ','
    _ <- newline
    return nums

number :: Parsec String () Integer
number = read <$> many1 digit

f :: Integer -> Integer -> [(Integer, Integer)]
f 0 v = [(6, v), (8, v)]
f x v = [(x - 1, v)]

simulate :: M.Map Integer Integer -> M.Map Integer Integer
simulate = M.fromListWith (+) . M.foldrWithKey (\k v l -> f k v ++ l) []

-- Integer has inf precision.
simulateN :: Integer -> M.Map Integer Integer -> M.Map Integer Integer
simulateN 0 m = m
simulateN n m = simulateN (n - 1) (simulate m)

main :: IO ()
main = do
  contents <- readFile "src/input-6.txt"
  let (Right initStates) = parse parser "" contents
  print . M.foldr (+) 0 . simulateN 256 $ M.fromListWith (+) . fmap (\a -> (a, 1)) $ initStates

main :: IO ()
main = do
  contents <- readFile "src/input-2.txt"
  print . solveFin . fmap (parseCustom . words) . lines $ contents
  print . solve2Fin . fmap (parseCustom . words) . lines $ contents

parseCustom :: [String] -> (String, Int)
parseCustom [x, y] = (x, read y)
parseCustom _ = error "should only be called with two arguments"

solve :: [(String, Int)] -> (Int, Int)
solve = foldl f (0, 0)
  where
    f (a, b) ("forward", v) = (a + v, b)
    f (a, b) ("up", v) = (a, b - v)
    f (a, b) ("down", v) = (a, b + v)
    f _ _ = error "not possible input"

solveFin :: [(String, Int)] -> Int
solveFin = multTup . solve
  where
    multTup (x, y) = x * y

solve2 :: [(String, Int)] -> (Int, Int, Int)
solve2 = foldl f (0, 0, 0)
  where
    f (a, b, c) ("forward", v) = (a + v, b + c * v, c)
    f (a, b, c) ("down", v) = (a, b, c + v)
    f (a, b, c) ("up", v) = (a, b, c - v)
    f _ _ = error "invalid input"

solve2Fin :: [(String, Int)] -> Int
solve2Fin = multTup . solve2
  where
    multTup (x, y, _) = x * y
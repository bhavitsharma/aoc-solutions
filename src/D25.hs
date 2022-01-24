{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (guard)
import Data.Array.Unboxed (UArray, accumArray, assocs, (!), (//))
import Data.Maybe (isJust)

solve :: [[Char]] -> Maybe Int
solve input = do
  let n = length input
      m = length (head input)
  let grid0 =
        accumArray @UArray
          (const id)
          '.'
          ((0, 0), (n - 1, m - 1))
          [((y, x), c) | (y, line) <- zip [0 ..] input, (x, c) <- zip [0 ..] line]
  let incX = (\(x, y) -> ((x + 1) `mod` n, y))
      incY = (\(x, y) -> (x, (y + 1) `mod` m))
      transform incF char grid = (not $ null changed, grid // changed)
        where
          changed = do
            (i, c) <- assocs grid
            guard $ c == char && grid ! incF i == '.'
            [(i, '.'), (incF i, c)]
      transform' state = s2 <$ guard (a || b)
        where
          (a, s1) = transform incY '>' state
          (b, s2) = transform incX 'v' s1
  pure $ length $ takeWhile isJust $ iterate (>>= transform') $ Just grid0

main :: IO ()
main = do
  contents <- readFile "src/input-25.txt"
  let moves = solve . lines $ contents
  print moves

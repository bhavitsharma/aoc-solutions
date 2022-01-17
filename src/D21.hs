import Control.Lens (set, view, _1, _2)
import Control.Monad (when)
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M

-- Dice Starting, (pos, ScoreA), (pos, ScoreB), noOfRolls
newDiceRoll i = 1 + (i `mod` 100)

newPlayerPosition i j = 1 + ((i - 1 + j) `mod` 10)

dice = [1, 2, 3]

rolls :: [(Int, Integer)]
rolls = M.toList . M.fromListWith (+) $ [(x + y + z, 1) | x <- dice, y <- dice, z <- dice]

waysL :: [Integer]
waysL = map snd rolls

type GState = ([(Int, Int)], Int)

type Cache = M.Map GState (Integer, Integer)

type GStateT = (GState, Cache)

getScore :: Int -> [(Int, Int)] -> Int
getScore i j = snd $ j !! i

getDefault 1 = (1, 0)
getDefault 0 = (0, 1)

createNewList a b position = if position == 0 then [a, b] else [b, a]

sum' (a1, b1) (a2, b2) (a3, b3) = (a1 + a2 + a3, b1 + b2 + b3)

sum'' (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

solve :: S.State GStateT (Integer, Integer)
solve = do
  (gameState@(players, position), cache) <- S.get
  case M.lookup gameState cache of
    Just val -> return val
    Nothing -> do
      let oPosition = 1 - position
      let (pos, score) = players !! position
      let o@(_, score') = players !! oPosition
      if score' >= 21
        then S.modify (set _2 . M.insert gameState (getDefault position) $ cache) >> (return . getDefault $ position)
        else do
          res <- mapM (\(s, ways) -> fmap (\(x, y) -> (x * ways, y * ways)) (createNewList' pos score s position o)) rolls
          let tf = foldr sum'' (0, 0) res
          newCache' <- S.gets (view _2)
          let newCache = M.insert gameState tf newCache'
          S.modify (set _2 newCache)
          return tf

createNewList' pos score roll position o = do
  let nScore = newScorePos pos score roll
  S.modify (set _1 (createNewList nScore o position, 1 - position))
  solve

newScorePos pos score roll = (x, sx)
  where
    x = newPlayerPosition pos roll
    sx = score + x

main :: IO ()
main = do
  print . fst $ S.runState solve (([(7, 0), (9, 0)], 0), M.empty)
import Control.Lens (set, view, _1, _2, _3, _4)
import qualified Control.Monad.State.Strict as S
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as St

data Var = X | Y | Z | W deriving (Ord, Eq, Show)

data VInt = V1 Var | V2 Int deriving (Eq, Show)

data Exp = Exp Var VInt deriving (Eq, Show)

data Op = Inp | Add Exp | Mul Exp | Div Exp | Mod Exp | Eql Exp deriving (Eq, Show)

getVar :: Op -> Var
getVar Inp = W
getVar (Add (Exp v _)) = v
getVar (Mul (Exp v _)) = v
getVar (Div (Exp v _)) = v
getVar (Mod (Exp v _)) = v
getVar (Eql (Exp v _)) = v

type VMap = M.Map Var Int

safeDiv :: Int -> Int -> Int
safeDiv a b =
  if a * b < 0
    then negate $ abs a `div` abs b
    else a `div` b

evalOp' :: (Int -> Int -> t) -> Exp -> M.Map Var Int -> t
evalOp' op (Exp v vi) m = v1 `op` v2
  where
    v1 = fromJust . M.lookup v $ m
    v2 = case vi of
      V1 variable -> fromJust . M.lookup variable $ m
      V2 constVar -> constVar

evalOp :: Op -> VMap -> Int
evalOp (Add e) = evalOp' (+) e
evalOp (Mul e) = evalOp' (*) e
evalOp (Div e) = evalOp' safeDiv e
evalOp (Mod e) = evalOp' mod e
evalOp (Eql e) = evalOp' (\a b -> fromEnum . (==) a $ b) e
evalOp Inp = error "invalid"

parseVar :: String -> Var
parseVar "x" = X
parseVar "y" = Y
parseVar "z" = Z
parseVar "w" = W
parseVar _ = error "undefined"

parseVInt :: String -> VInt
parseVInt "x" = V1 X
parseVInt "y" = V1 Y
parseVInt "z" = V1 Z
parseVInt "w" = V1 W
parseVInt s = V2 . read $ s

getExp :: [String] -> Exp
getExp [a, b] = Exp (parseVar a) (parseVInt b)
getExp _ = error "Should have only two arguments"

createOp :: String -> Op
createOp s
  | length s' == 2 = Inp
  | first == "add" = Add expr
  | first == "mul" = Mul expr
  | first == "div" = Div expr
  | first == "eql" = Eql expr
  | first == "mod" = Mod expr
  | otherwise = error "unknown operator"
  where
    s' = words s
    first = head s'
    rest = drop 1 s'
    expr = getExp rest

removeInp :: [Op] -> [[Op]]
removeInp = wordsBy (== Inp)

evalInp' :: [Op] -> S.State VMap ()
evalInp' [] = return ()
evalInp' (x : xs) = do
  map <- S.get
  let value = evalOp x map
  S.modify (M.insert (getVar x) value)
  evalInp' xs

type VisState = (Int, VMap, St.Set (Int, Int), Int)

loopUntilTrue :: Int -> [(Int, VMap)] -> [[Op]] -> S.State VisState Bool
loopUntilTrue _ [] _ = return False
loopUntilTrue index (x : xs) ns = do
  let (num, map_) = x
  res <- S.modify (set _1 (index + 1)) >> S.modify (set _2 map_) >> S.modify (set _4 num) >> getAllStates ns
  if res
    then return True
    else do
      loopUntilTrue index xs ns

getAllStates :: [[Op]] -> S.State VisState Bool
getAllStates [] = do
  (index, m, _, _) <- S.get
  let zValue = fromJust . M.lookup Z $ m
  return (index == 14 && zValue == 0)
getAllStates (x : xs) = do
  (index, m, visited, val) <- S.get
  let zValue = fromJust . M.lookup Z $ m
  let lst = (index, zValue)
  if St.member lst visited
    then return (St.member (14, 0) visited)
    else do
      S.modify (set _3 $ St.insert lst visited)
      -- remove reverse to solve the second part.
      let newMaps = reverse [(val * 10 + i, S.execState (evalInp' x) $ M.insert W i m) | i <- [1 .. 9]]
      loopUntilTrue index newMaps xs

main :: IO ()
main = do
  contents <- readFile "src/input-24.txt"
  let a = removeInp . fmap createOp . lines $ contents
  print (head a)
  print . view _4 $ S.execState (getAllStates (take 14 a)) (0, M.fromList $ [(Y, 0), (Z, 0), (X, 0)], St.empty, 0)

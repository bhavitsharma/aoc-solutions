{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (ap, liftM) -- you will need to put this towards the top of the file

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

-- s === State, a === Output
newtype State s a = State {runState :: s -> (a, s)}

-- \s -> (b, s)
-- a -> (s -> (b, s))

instance Monad (State s) where
  return :: a -> State s a
  return x = State (x,) -- from TupleSections

  (>>=) :: State s a -> (a -> State s b) -> State s b
  p >>= k = State $ \s ->
    let (a, s_1) = runState p s
     in runState (k a) s_1

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = State coin
pushS = State push

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]
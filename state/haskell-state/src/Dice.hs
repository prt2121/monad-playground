module Dice where

import Control.Monad.Trans.State
import Control.Monad
import System.Random
-- Control.Monad.State  (normally used this one)

-- type State describes functions that consume a state and produce both a result and an updated state.
--     newtype State s a = State { runState :: s -> (a, s) }
--     's' is the type of the state, and 'a' the type of the produced result
--     State should be called state processor

-- constructor
-- state :: (s -> (a, s)) -> State s a

-- bind (>>=)
-- It's like function application,
-- it takes a monadic value
-- and feeds it to a function that takes a normal value but returns a monadic value.
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- The StdGen type we are using is an instance of RandomGen.
-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)

rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)

rollDie' :: State StdGen Int
rollDie' = do generator <- get
              let (value, newGenerator) = randomR (1,6) generator
              put newGenerator
              return value

rollDice :: State StdGen (Int, Int)
rollDice = liftM2 (,) rollDie rollDie

rollNDice :: Int -> State StdGen [Int]
rollNDice n = replicateM n rollDie

-- *Dice> evalState rollDie (mkStdGen 0)
-- 6

-- *Dice> evalState rollDice (mkStdGen 0)
-- (6,6)

-- *Dice> evalState (rollNDice 5) (mkStdGen 0)
-- [6,6,4,1,5]

-- *Dice> evalState (rollNDice 5) (mkStdGen 10)
-- [6,4,6,1,2]
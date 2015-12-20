-- http://learnyouahaskell.com/for-a-few-monads-more
module Stack where

import Control.Monad.State

type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)

push' :: Int -> Stack -> ((),Stack)
push' n s = ((), n:s)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

popPop :: State Stack [Int]
popPop = do
    a <- pop
    b <- pop
    return [a, b]

-- *Stack> let s = [0, 1, 2, 3, 4, 5]
-- *Stack> runState popPop s
-- ([0,1],[2,3,4,5])
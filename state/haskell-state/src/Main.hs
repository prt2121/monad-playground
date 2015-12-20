module Main where

import Control.Monad.State
import System.Random

play :: State Int Int
play = do
    put 7
    modify (*3)
    get

-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State
-- rolls two dice , given a generator,
-- return a tuple with our random numbers as first element and the last generator as the second.
rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g = ((n, m), gen)
                where
                    (n, g')  = randomR (1,6) g
                    (m, gen) = randomR (1,6) g'
-- *Main> rollDice (mkStdGen 2)
-- ((6,4),508393462 1655838864)
-- *Main> rollDice (mkStdGen 3)
-- ((6,4),2109513658 1655838864)



main :: IO ()
main = print $ execState play 0

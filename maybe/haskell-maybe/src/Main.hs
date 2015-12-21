module Main where

-- http://learnyouahaskell.com/a-fistful-of-monads
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe

--  fmap    :: (Functor f)      => (a -> b)     -> f a          -> f b
--  (<*>)   :: (Applicative f)  => f (a -> b)   -> f a          -> f b
--  (>>=)   :: (Monad m)        => m a          -> (a -> m b)   -> m b

-- Maybe instance
--
-- return :: a -> Maybe a
--     return x  = Just x
--
-- (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
--     (>>=) m g = case m of
--                    Nothing -> Nothing
--                    Just x  -> g x

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
    | x > 0    = Just (log x)
    | otherwise = Nothing

safeLogSqrt = safeLog <=< safeSqrt

unsafeLogSqrt = log . sqrt

-- monad composition operator
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
--    f >=> g = \x -> f x >>= g

-- Walk the line

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft  b (l, r)
    | abs (b + l - r) < 4   = Just (b + l, r)
    | otherwise             = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight b (l, r)
    | abs (b + r -l) < 4    = Just (l, b + r)
    | otherwise             = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

routine :: Maybe Pole
routine =
    case Just (0,0) of
        Nothing -> Nothing
        Just start -> case landLeft 2 start of
            Nothing -> Nothing
            Just first -> case landRight 2 first of
                Nothing -> Nothing
                Just second -> landLeft 1 second

routine' :: Maybe Pole
routine' = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

main :: IO ()
main = do
  putStrLn "maybe..."
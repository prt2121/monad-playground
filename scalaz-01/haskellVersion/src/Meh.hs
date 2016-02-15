module Meh where
-- JavaScript-ish yes-no typeclass

-- yesno takes one value of a type that can be considered to hold some concept of true-ness
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing  = False
  yesno (Just _) = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

main :: IO ()
main = do
  putStrLn "hello world"

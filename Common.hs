module Common where

doNTimes :: (a -> a) -> a -> Int -> a
doNTimes func init times
   | times > 0 = iterate func init !! times
   | otherwise = init

doWhile :: (a -> a) -> (a -> Bool) -> a -> a
doWhile func cond init
   | cond init = doWhile func cond (func init)
   | otherwise = init

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy cond input
   | null rest = [value]
   | otherwise = value:splitBy cond (tail rest)
   where (value, rest) = break cond input

between :: Ord a => a -> a -> a -> Bool
between value low high = value >= low && value <= high

extractMaybe :: Maybe a -> a
extractMaybe (Just x) = x

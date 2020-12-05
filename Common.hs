module Common where

import Data.List ( foldl' )
import Data.Bifunctor ( bimap )

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

-- Need separate accessors in this because it'll deduce the type to (a, a) -> a and (b, b) -> b
-- So, an ugly and unfortunate workaround
foldlTuple :: (b -> a -> b) -> (b, b) -> [(a, a)] -> (b, b)
foldlTuple func init fold_over = (doFold fst fst, doFold snd snd)
   where doFold accessor1 accessor2 = foldl' func (accessor1 init) $ accessor2 <$> fold_over

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f = bimap f f

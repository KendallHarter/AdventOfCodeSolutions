import Data.List ( sort, elemIndex )
import Common ( extractMaybe )

getGrouping :: Int -> [Int] -> [Int]
getGrouping loc input = fmap fst $ takeWhile ((==(-1)) . uncurry (-)) $ makeList loc input
   where makeList loc input = zip (subList loc input) (subList (loc + 1) input)
         subList loc input = drop loc input

-- The number of possible orderings given a group size
countForGroupSize :: (Num a, Eq a) => a -> a
countForGroupSize 0 = 1
countForGroupSize 1 = 1
countForGroupSize 2 = 2
countForGroupSize 3 = 4
countForGroupSize 4 = 7

main :: IO ()
main = do
   input <- getContents
   let preRows = sort (read <$> lines input) :: [Int]
   let rows = 0:preRows ++ [last preRows + 3]
   let num1 = length $ filter ((==(-1)) . uncurry (-)) $ zip rows $ tail rows
   let names3 = fmap fst $ filter ((==(-3)) . uncurry (-)) $ zip rows $ tail rows
   let loc3 = (+1) . extractMaybe . (`elemIndex` rows) <$> names3
   let num3 = length $ filter ((==(-3)) . uncurry (-)) $ zip rows $ tail rows
   print rows
   putStr "Part 1: "
   print $ num1 * num3
   putStr "Part 2: "
   print $ product $ countForGroupSize <$> (length . (`getGrouping` rows) <$> 0:loc3)

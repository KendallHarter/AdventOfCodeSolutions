doSingleWeight :: Int -> Int
doSingleWeight x = div x 3 - 2

makeWeightArray :: Int -> [Int]
makeWeightArray x
   | weight <= 0 = [0]
   | otherwise   = weight:makeWeightArray weight
   where weight = doSingleWeight x

main :: IO ()
main = do
   input <- getContents
   let rows = read <$> lines input :: [Int]
   putStr "Part 1: "
   print $ sum $ doSingleWeight <$> rows
   putStr "Part 2: "
   print $ sum $ sum . makeWeightArray <$> rows

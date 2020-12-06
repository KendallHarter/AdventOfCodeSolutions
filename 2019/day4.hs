import Common ( splitBy )

mapSelf :: (a -> a -> b) -> [a] -> [b]
mapSelf f x = uncurry f <$> zip x (tail x)

main :: IO ()
main = do
   input <- getContents
   let (min:max:_) = fmap read $ splitBy (=='-') $ head $ lines input :: [Int]
   let increasing = and . mapSelf (<=) :: String -> Bool
   let equivAdjacent = or . mapSelf (==) :: String -> Bool
   let inputs = fmap show [min..max]
   putStr "Part 1: "
   print $ length $ filter (\x -> increasing x && equivAdjacent x) inputs
   let nextIsEquiv = mapSelf (==) :: String -> [Bool]
   let hasDoubleGroup = any ((==1) . length) . splitBy not . nextIsEquiv
   putStr "Part 2: "
   print $ length $ filter (\x -> increasing x && hasDoubleGroup x) inputs

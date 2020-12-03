pairWithRest :: [a] -> [(a, a)]
pairWithRest (x:rest) = map (\y -> (x,y)) rest
pairWithRest _ = []

allPairs :: [a] -> [(a, a)]
allPairs (x:rest) = pairWithRest (x:rest) ++ allPairs rest
allPairs _ = []

makeTriple :: [a] -> [(a, a, a)]
makeTriple (x1:x2:rest) = map (\(val1, val2) -> (x1, val1, val2)) $ allPairs (x2:rest)
makeTriple _ = []

allTriples :: [a] -> [(a, a, a)]
allTriples (x1:x2:rest) = makeTriple (x1:x2:rest) ++ allTriples (x2:rest)
allTriples _ = []

main :: IO ()
main = do
   input <- getContents
   let numbers = map read $ lines input :: [Int]
   putStr "Part 1: "
   print $ head [a * b | (a, b) <- allPairs numbers, a + b == 2020]
   putStr "Part 2: "
   print $ head [a * b * c | (a, b, c) <- allTriples numbers, a + b + c == 2020]

stepByN :: Int -> [a] -> [a]
stepByN n list = fst <$> filter (\(_, x) -> mod x n == 0) (zip list [0..])

treeCount :: [String] -> Int -> Int -> Int
treeCount input x_step y_step
   = length $ filter (=='#') $ uncurry (!!) <$> zip (stepByN y_step input) (0:[x_step, x_step*2..])

main :: IO()
main = do
   input <- getContents
   let rows = cycle <$> lines input
   putStr "Part1: "
   print $ treeCount rows 3 1
   let offsets = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
   let do_tree_count = treeCount rows
   putStr "Part2: "
   print $ product $ uncurry do_tree_count <$> offsets

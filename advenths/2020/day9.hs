import Data.Bifunctor ( bimap )

main :: IO ()
main = do
   input <- getContents
   let rows = read <$> lines input :: [Int]
   let last25Sum base = uncurry (+) . bimap (rows!!) (rows!!) <$> [(x, y) | x <- [base..base + 23], y <- [base + 1..base + 24]]
   let occurs loc = rows!!(loc + 25) `elem` last25Sum loc
   let insecureNum = rows!!(25 + head (dropWhile occurs [0..]))
   putStr "Part 1: "
   print insecureNum
   putStr "Part 2: "
   let sums (loc, len) = (sum $ (rows!!) <$> [loc..loc + len], (loc, len))
   let listsFor x = zip [0..length rows - x - 1] $ repeat x
   let (_, (loc, len)) = head $ head $ dropWhile null $ dropWhile ((/=insecureNum) . fst) <$> (fmap sums <$> (listsFor <$> [2..]))
   let nums = take (len + 1) $ drop loc rows
   print $ minimum nums + maximum nums

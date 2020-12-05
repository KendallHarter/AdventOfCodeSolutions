import Data.List ( foldl', sort )

doBinaryPartitionInternal :: String -> Char -> Char -> Int -> Int -> Int
doBinaryPartitionInternal [c] low_char high_char low high
   | c == low_char  = low
   | c == high_char = high - 1
doBinaryPartitionInternal (c:rest) low_char high_char low high
   | c == low_char  = doBinaryPartitionInternal rest low_char high_char low next_val
   | c == high_char = doBinaryPartitionInternal rest low_char high_char next_val high
      where next_val = low + div (high - low) 2

doBinaryPartition :: String -> Char -> Char -> Int
doBinaryPartition input low_char high_char
   = doBinaryPartitionInternal input low_char high_char 0 $ 2 ^ length input

getRowCol :: String -> (Int, Int)
getRowCol input = (doBinaryPartition (fst row_col) 'F' 'B', doBinaryPartition (snd row_col) 'L' 'R')
   where row_col = break (\x -> x == 'R' || x == 'L') input

calcSeatId :: (Int, Int) -> Int
calcSeatId (row, col) = row * 8 + col

main :: IO ()
main = do
   input <- getContents
   let locs = getRowCol <$> lines input
   let ids = sort $ calcSeatId <$> locs
   putStr "Part 1: "
   print $ foldl' max 0 ids
   putStr "Part 2: "
   print $ (+1) $ fst $ head $ filter (\(id1, id2) -> id2 - id1 == 2) $ zip ids $ tail ids

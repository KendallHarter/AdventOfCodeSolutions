-- import Data.List ( foldl1' )
import Data.Array.IArray ( array, (!), (//), elems, indices  )
import Data.Array.Unboxed ( UArray )
import Data.List ( foldl1' )
import Debug.Trace

-- I really need to figure out how to make these work with arrays...
-- data Seat = Empty | Filled | Floor
--    deriving Enum

-- toSeat :: Char -> Seat
-- toSeat 'L' = Empty
-- toSeat '#' = Filled
-- toSeat '.' = Floor

-- instance Show Seat where
--    show Empty = "L"
--    show Filled = "#"
--    show Floor = "."

updateCell :: UArray (Int, Int) Char -> (Int, Int) -> ((Int, Int), Char)
updateCell seats loc
   | seats!loc == '.' = (loc, '.')
   | seats!loc == 'Z' = (loc, 'Z')
   | numAdjacent == 0 = (loc, '#')
   | numAdjacent >= 4 = (loc, 'L')
   | otherwise        = (loc, seats!loc)
   where numAdjacent = adjust + length (filter (=='#') $ (seats!) <$> adjLocs)
         adjust = if seats!loc == '#' then -1 else 0
         (x, y) = loc
         adjLocs = [(x2, y2) | x2 <- [x - 1..x + 1], y2 <- [y - 1..y + 1]]

stepSeats :: UArray (Int, Int) Char -> UArray (Int, Int) Char
stepSeats seats = seats // updates
   where updates = uncurry updateCell <$> zip (repeat seats) (indices seats)

updateCell2 :: UArray (Int, Int) Char -> (Int, Int) -> ((Int, Int), Char)
updateCell2 seats loc
   | seats!loc == '.' = (loc, '.')
   | seats!loc == 'Z' = (loc, 'Z')
   | numAdjacent == 0 = (loc, '#')
   | numAdjacent >= 5 = (loc, 'L')
   | otherwise        = (loc, seats!loc)
   where numAdjacent = adjust + length (filter (=='#') $ (seats!) <$> adjLocs)
         adjust = if seats!loc == '#' then -1 else 0
         adjLocs = [findLoc (x2, y2) | x2 <- [-1..1], y2 <- [-1..1]]
         addOff  (offX, offY) (startX, startY) = (startX + offX, startY + offY)
         findLoc (0, 0) = loc
         findLoc offsets = head $ dropWhile (\l -> l == loc || seats!l == '.') $ iterate (addOff offsets) loc

stepSeats2 :: UArray (Int, Int) Char -> UArray (Int, Int) Char
stepSeats2 seats = seats // updates
   where updates = uncurry updateCell2 <$> zip (repeat seats) (indices seats)

debugSeats :: UArray (Int, Int) Char -> Int -> String
debugSeats seats width = foldl1' (\x y -> x ++ "\n" ++ y) $ takeWhile (/="") $ getSlice <$> 0:[width, width * 2..]
   where getSlice start = take width . drop start $ elems seats

main :: IO ()
main = do
   input <- getContents
   -- Z is border
   let baseSeats = (\x -> 'Z':x ++ "Z") <$> lines input
   let height = 2 + length baseSeats
   let width = length $ head baseSeats
   let padding = replicate width 'Z'
   let seatsList = padding:baseSeats ++ [padding]
   let seats = array ((0, 0), (width - 1, height - 1)) [((x, y), seatsList!!y!!x) | x <- [0..width - 1], y <- [0..height - 1]] :: UArray (Int, Int) Char
   let firstStep = stepSeats seats
   let getSeats = fst . head . dropWhile (uncurry (/=))
   let finalState = getSeats $ zip (iterate stepSeats seats) $ iterate stepSeats firstStep
   putStr "Part 1: "
   print $ length $ filter (=='#') $ elems finalState
   let finalState2 = getSeats $ zip (iterate stepSeats2 seats) $ iterate stepSeats2 (stepSeats2 seats)
   putStr "Part 2: "
   print $ length $ filter (=='#') $ elems finalState2

-- import Common ( splitBy, foldlTuple )
-- import Data.Maybe ( isJust )
-- import Data.List ( elemIndex, foldl' )
-- import Data.Array ( Array, array, (!), (//) )
-- import Debug.Trace ( trace )
-- import Data.Bifunctor ( bimap )

-- applyMove :: String -> (Int, Int) -> [(Int, Int)]
-- applyMove ('R':rest) (px, py) = [(px + x, py) | x <- [1..read rest]]
-- applyMove ('L':rest) (px, py) = [(px - x, py) | x <- [1..read rest]]
-- applyMove ('D':rest) (px, py) = [(px, py + y) | y <- [1..read rest]]
-- applyMove ('U':rest) (px, py) = [(px, py - y) | y <- [1..read rest]]

-- makeWiresInternal :: (Int, Int) -> [String] -> [(Int, Int)]
-- makeWiresInternal prev (move:next_moves) = wires ++ makeWiresInternal (last wires) next_moves
--    where wires = applyMove move prev
-- makeWiresInternal _ [] = []

-- makeWires :: [String] -> [(Int, Int)]
-- makeWires = makeWiresInternal (0, 0)

-- minMax :: Ord a => a -> a -> (a, a)
-- minMax v1 v2 = (min v1 v2, max v1 v2)

-- updateMinMaxPair :: Ord a => (a, a) -> a -> (a, a)
-- updateMinMaxPair (old_min, old_max) new_val = (min old_min new_val, max old_max new_val)

-- mergeMinMaxPairs :: Ord a => (a, a) -> (a, a) -> (a, a)
-- mergeMinMaxPairs (min1, max1) (min2, max2) = (min min1 min2, max max1 max2)

-- -- [(int, int)] -> ((int, int), (int, int))
-- -- ((int, int), (int, int)) -> ((int, int), (int, int)) -> ((int, int), (int, int))
-- --    ((1, 2), (3, 4)) -> ((0, 6), (5, 8)) -> ((1, 6), (3, 8))
-- --    ((min 1 0, max 2 6), (min 3 3, max 4 8))
-- --    (mergeMinMaxPairs (1, 2) (0, 6), mergeMinMaxPairs (2, 6) (3, 8))

-- adjustLoc :: Num a => (a, a) -> (a, a) -> (a, a)
-- adjustLoc (minx, miny) (x, y) = (x - minx, y - miny)

-- main :: IO ()
-- main = do
--    input <- getContents
--    let wires = makeWires . splitBy (==',') <$> lines input
--    let wire1 = head wires
--    let wire2 = last wires
--    let calcWireBound = foldlTuple updateMinMaxPair ((maxBound, minBound), (maxBound, minBound))
--    let x = calcWireBound wire1
--    print $ calcWireBound wire1
--    print $ calcWireBound wire2
--    -- I'm sure there's a better way of doing this but I can't think of it...
--    let (wire1x, wire1y) = calcWireBound wire1
--    let (wire2x, wire2y) = calcWireBound wire2
--    let wireBounds = (mergeMinMaxPairs wire1x wire2x, mergeMinMaxPairs wire1y wire2y)
--    let (xBounds, yBounds) = wireBounds
--    let minBounds = (fst xBounds, fst yBounds)
--    let makeBounds bounds = [fst bounds .. snd bounds]
--    let adjusted_wire1 = adjustLoc minBounds <$> wire1
--    let adjusted_wire2 = adjustLoc minBounds <$> wire2
--    let adjBounds = bimap (adjustLoc minBounds) (adjustLoc minBounds) wireBounds
--    let xs = makeBounds xBounds
--    let ys = makeBounds yBounds
--    let firstWireMap = array adjBounds [((x, y), False) | x <- xs, y <- ys] // zip adjusted_wire1 (repeat True)
--    let collisions = filter (\x -> trace (show x) $ firstWireMap!x) adjusted_wire2
--    print collisions

main :: IO ()
main = putStrLn "Given up on this due to errors with array that I don't understand"

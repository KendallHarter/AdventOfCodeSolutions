import Common ( splitBy, foldlTuple, extractMaybe )
import Data.List ( elemIndex, foldl' )
import Data.Array ( array, (!), (//) )
import Data.Bifunctor ( bimap )
import Debug.Trace ( trace )

applyMove :: String -> (Int, Int) -> [(Int, Int)]
applyMove ('R':rest) (px, py) = [(px + x, py) | x <- [1..read rest]]
applyMove ('L':rest) (px, py) = [(px - x, py) | x <- [1..read rest]]
applyMove ('D':rest) (px, py) = [(px, py + y) | y <- [1..read rest]]
applyMove ('U':rest) (px, py) = [(px, py - y) | y <- [1..read rest]]

makeWiresInternal :: (Int, Int) -> [String] -> [(Int, Int)]
makeWiresInternal prev (move:next_moves) = wires ++ makeWiresInternal (last wires) next_moves
   where wires = applyMove move prev
makeWiresInternal _ [] = []

makeWires :: [String] -> [(Int, Int)]
makeWires = makeWiresInternal (0, 0)

minMax :: Ord a => a -> a -> (a, a)
minMax v1 v2 = (min v1 v2, max v1 v2)

updateMinMaxPair :: Ord a => (a, a) -> a -> (a, a)
updateMinMaxPair (old_min, old_max) new_val = (min old_min new_val, max old_max new_val)

mergeMinMaxPairs :: Ord a => (a, a) -> (a, a) -> (a, a)
mergeMinMaxPairs (min1, max1) (min2, max2) = (min min1 min2, max max1 max2)

adjustLoc :: Num a => (a, a) -> (a, a) -> (a, a)
adjustLoc (minx, miny) (x, y) = (x - minx, y - miny)

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
   input <- getContents
   let wires = makeWires . splitBy (==',') <$> lines input
   let wire1 = head wires
   let wire2 = last wires
   let calcWireBound = foldlTuple updateMinMaxPair ((maxBound, minBound), (maxBound, minBound))
   let (wire1x, wire1y) = calcWireBound wire1
   let (wire2x, wire2y) = calcWireBound wire2
   -- Need to adjust 2D to 1D with index 0 as otherwise array errors internally
   let wireBounds = (mergeMinMaxPairs wire1x wire2x, mergeMinMaxPairs wire1y wire2y)
   let minBounds = bimap fst fst wireBounds
   let adjustedWire1 = adjustLoc minBounds <$> wire1
   let adjustedWire2 = adjustLoc minBounds <$> wire2
   let (adjXBounds, adjYBounds) = bimap ((,) 0 . (\x -> snd x - fst minBounds)) ((,) 0 . (\x -> snd x - snd minBounds)) wireBounds :: ((Int, Int), (Int, Int))
   let width = snd adjXBounds + 1
   let flattenBounds = \(x, y) -> x + y * width
   let flattenedBounds = (0, width * (snd adjYBounds + 1))
   let firstWireMap = array flattenedBounds [(i, False) | i <- [0 .. snd flattenedBounds]] // zip (flattenBounds <$> adjustedWire1) (repeat True)
   let collisions = filter ((firstWireMap!) . flattenBounds) adjustedWire2
   -- Need to negate minimum bounds for distance to be calculated properly
   putStr "Part 1: "
   print $ foldl' min maxBound $ dist (bimap negate negate minBounds) <$> collisions
   -- Go through all collisions and find the number of steps for each wire to
   -- get to that
   let collisionLoc = \wire collision -> extractMaybe $ elemIndex collision wire
   let stepsOne = collisionLoc adjustedWire1 <$> collisions
   let stepsTwo = collisionLoc adjustedWire2 <$> collisions
   putStr "Part 2: "
   -- Need to add two since this is one-indexed
   print $ (+) 2 $ foldl' min maxBound $ uncurry (+) <$> zip stepsOne stepsTwo

import Data.List ( foldl' )

data Direction = North | South | East | West
   deriving Eq

data Instruction = Forward Int | Right' Int | Left' Int | Dir Direction Int
   deriving Eq

readInstr :: String -> Instruction
readInstr ('F':rest) = Forward $ read rest
readInstr ('R':rest) = Right' $ read rest
readInstr ('L':rest) = Left' $ read rest
readInstr ('N':rest) = Dir North $ read rest
readInstr ('S':rest) = Dir South $ read rest
readInstr ('E':rest) = Dir East $ read rest
readInstr ('W':rest) = Dir West $ read rest

data Ship = Ship
   { x        :: Int
   , y        :: Int
   , facing   :: Direction
   , waypoint :: (Int, Int)
   }

initialShip :: Ship
initialShip = Ship 0 0 East (10, 1)

rotateRight :: Direction -> Int -> Direction
rotateRight dir degrees = dropWhile (/=dir) (cycle [North, East, South, West]) !! (degrees `div` 90)

executeInstr :: Ship -> Instruction -> Ship
executeInstr ship instr = case instr of
   Forward n -> executeInstr ship $ Dir (facing ship) n
   Right' n  -> ship { facing = rotateRight (facing ship) n }
   Left' n   -> ship { facing = rotateRight (facing ship) (360 - n) }
   Dir d n   -> case d of
      North -> ship { y = y ship + n }
      South -> ship { y = y ship - n }
      East  -> ship { x = x ship + n }
      West  -> ship { x = x ship - n }

-- I'm sure there's a smarter way to do this
rotateWaypointRight :: Ship -> Ship
rotateWaypointRight ship = ship { waypoint = (y, -x) }
   where (x, y) = waypoint ship

executeInstr2 :: Ship -> Instruction -> Ship
executeInstr2 ship instr = case instr of
   Forward n -> ship { x = x ship + wpx * n, y = y ship + wpy * n }
   Right' n  -> iterate rotateWaypointRight ship !! (n `div` 90)
   Left'  n  -> iterate rotateWaypointRight ship !! ((360 - n) `div` 90)
   Dir d n   -> case d of
      North -> ship { waypoint = (wpx, wpy + n) }
      South -> ship { waypoint = (wpx, wpy - n) }
      East  -> ship { waypoint = (wpx + n, wpy) }
      West  -> ship { waypoint = (wpx - n, wpy) }
   where (wpx, wpy) = waypoint ship

main :: IO ()
main = do
   input <- getContents
   let instrs = readInstr <$> lines input
   let finalShip = foldl' executeInstr initialShip instrs
   let getAnswer ship = abs (x ship) + abs (y ship)
   putStr "Part 1: "
   print $ getAnswer finalShip
   putStr "Part 2: "
   print $ getAnswer $ foldl' executeInstr2 initialShip instrs
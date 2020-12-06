import Data.Array ( Array, array, (!), (//) )
import Common ( splitBy )

data Program = Program
   { memory   :: Array Int Int
   , input    :: [Int]
   , output   :: [Int]
   , isHalted :: Bool
   , instrPtr :: Int
   }

data Instr = Instr
   { op        :: Int
   , accessors :: [Array Int Int -> Int -> Int]
   }

-- Should probably have opcodes be named, but eh

doOpcode :: Instr -> Program -> Program
doOpcode instr prog = result
   where new_loc = 1 + instrPtr prog + argCnt
         mem    = memory prog
         getArg = uncurry <$> accessors instr
         argCnt = length (accessors instr)
         args   = (\(n1, n2) -> n1 n2) <$> zip getArg (zip (repeat mem) $ fmap (mem!) [instrPtr prog + 1..instrPtr prog + argCnt + 1])
         in'    = input prog
         out    = output prog
         -- There's probably a better way to do this...
         result = case op instr of
            1  -> Program (mem // [(args!!2, head args + args!!1)]) in' out False new_loc
            2  -> Program (mem // [(args!!2, head args * args!!1)]) in' out False new_loc
            3  -> Program (mem // [(head args, head in')]) (tail in') out False new_loc
            4  -> Program mem in' (out ++ args) False new_loc
            5  -> Program mem in' out False $ if head args /= 0 then args!!1 else new_loc
            6  -> Program mem in' out False $ if head args == 0 then args!!1 else new_loc
            7  -> Program (mem // [(args!!2, if head args < args!!1 then 1 else 0)]) in' out False new_loc
            8  -> Program (mem // [(args!!2, if head args == args!!1 then 1 else 0)]) in' out False new_loc
            99 -> Program mem in' out True new_loc

makeAccessor :: Int -> (Array Int Int -> Int -> Int)
makeAccessor 0 = (!)      -- deref
makeAccessor 1 = const id -- immediate

-- This is my dumb workaround for the fact that parameters that write to memory
-- are labeled as position mode but function as immediate mode
getOpInfo :: Int -> [Bool]
getOpInfo  1 = [False, False, True]
getOpInfo  2 = [False, False, True]
getOpInfo  3 = [True]
getOpInfo  4 = [False]
getOpInfo  5 = [False, False]
getOpInfo  6 = [False, False]
getOpInfo  7 = [False, False, True]
getOpInfo  8 = [False, False, True]
getOpInfo 99 = []

parseOpcode :: Program -> Instr
parseOpcode prog = Instr opCode access
   where rawData   = memory prog ! instrPtr prog
         opCode    = rawData `mod` 100
         getAccess = \divisor -> makeAccessor $ (rawData `div` divisor) `mod` 10
         isWrite   = getOpInfo opCode
         -- This is before being cleaned up
         access'   = take (length isWrite) $ getAccess <$> iterate (*10) 100
         -- Force write modes to be tagged as immediate
         access    = (\(b, (x, y)) -> if b then x else y) <$> zip isWrite (zip (repeat $ makeAccessor 1) access')

runProgramInternal :: Program -> Program
runProgramInternal prog
   | isHalted prog = prog
   | otherwise     = runProgramInternal $ doOpcode (parseOpcode prog) prog

runProgram :: Array Int Int -> [Int] -> Program
runProgram memory input = runProgramInternal $ Program memory input [] False 0

main :: IO ()
main = do
   input <- getContents
   let prog = read <$> splitBy (==',') (head $ lines input) :: [Int]
   let prog_array = array (0, length prog - 1) (zip [0..] prog)
   putStr "Part 1: "
   print $ last $ output $ runProgram prog_array [1]
   putStr "Part 2: "
   print $ head $ output $ runProgram prog_array [5]

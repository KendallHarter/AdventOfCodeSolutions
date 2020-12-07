module Intcode
   ( runProgram, readProgram, Program, memory, input
   , output, isHalted, instrPtr, initProg, runUntilOutput
   ) where

import Data.Array ( Array, array, (!), (//) )
import Common ( splitBy )

data Program = Program
   { memory   :: Array Int Int
   , input    :: [Int]
   , output   :: [Int]
   , isHalted :: Bool
   , instrPtr :: Int
   }

instance Show Program where
   show (Program _ in_ out halt done)
      =  "Program {Input: " ++ show in_ ++ "; "
      ++ "Output: " ++ show out ++ "; "
      ++ "Is Halted: " ++ show halt ++ "; "
      ++ "Instruction Pointer: " ++ show done ++ "}"

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
            1  -> prog { memory = mem // [(args!!2, head args + args!!1)], instrPtr = new_loc }
            2  -> prog { memory = mem // [(args!!2, head args * args!!1)], instrPtr = new_loc }
            3  -> prog { memory = mem // [(head args, head in')], input = tail in', instrPtr = new_loc}
            4  -> prog { output = out ++ args, instrPtr = new_loc }
            5  -> prog { instrPtr = if head args /= 0 then args!!1 else new_loc }
            6  -> prog { instrPtr = if head args == 0 then args!!1 else new_loc }
            7  -> prog { memory = mem // [(args!!2, if head args < args!!1 then 1 else 0)], instrPtr = new_loc }
            8  -> prog { memory = mem // [(args!!2, if head args == args!!1 then 1 else 0)], instrPtr = new_loc }
            99 -> prog { isHalted = True }

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

runUntilOutput :: Program -> Maybe (Program, Int)
runUntilOutput prog
   | isHalted prog      = Nothing
   | null $ output prog = runUntilOutput $ doOpcode (parseOpcode prog) prog
   | otherwise          = Just (prog { output = [] }, head $ output prog)

initProg :: Array Int Int -> [Int] -> Program
initProg memory input = Program memory input [] False 0

readProgram :: IO (Array Int Int)
readProgram = do
   input <- getContents
   let prog = read <$> splitBy (==',') (head $ lines input) :: [Int]
   return $ array (0, length prog - 1) (zip [0..] prog)

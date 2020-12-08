module Intcode
   ( runProgram, readProgram, Program, memory, input
   , output, isHalted, instrPtr, initProg, runUntilOutput
   ) where

-- Probably want to rewrite this in terms of IOUArray?

import Data.Array.IArray ( array, (!), (//), elems )
import Data.Array.Unboxed ( UArray )
import Common ( splitBy )

data Program = Program
   { memory       :: UArray Int Int
   , input        :: [Int]
   , output       :: [Int]
   , isHalted     :: Bool
   , instrPtr     :: Int
   , relativeBase :: Int
   }

instance Show Program where
   show (Program _ in_ out halt done relative)
      =  "Program {Input: " ++ show in_ ++ "; "
      ++ "Output: " ++ show out ++ "; "
      ++ "Is Halted: " ++ show halt ++ "; "
      ++ "Instruction Pointer: " ++ show done ++ "; "
      ++ "Relative Base: " ++ show relative ++ "}"

data Instr = Instr
   { op        :: Int
   , accessors :: [Program -> Int -> Int]
   }

-- Should probably have opcodes be named, but eh

doOpcode :: Instr -> Program -> Program
doOpcode instr prog =result
   where new_loc = 1 + instrPtr prog + argCnt
         mem    = memory prog
         getArg = uncurry <$> accessors instr
         argCnt = length (accessors instr)
         args   = (\(n1, n2) -> n1 n2) <$> zip getArg (zip (repeat prog) $ fmap (mem!) [instrPtr prog + 1..instrPtr prog + argCnt])
         raw    = fmap (mem!) [instrPtr prog + 1..instrPtr prog + argCnt]
         in'    = input prog
         out    = output prog
         -- There's probably a better way to do this...
         result = case op instr of
            1  -> prog { memory = mem // [(args!!2, head args + args!!1)], instrPtr = new_loc }
            2  -> prog { memory = mem // [(args!!2, head args * args!!1)], instrPtr = new_loc }
            3  -> prog { memory = mem // [(head args, head in')], input = tail in', instrPtr = new_loc }
            4  -> prog { output = out ++ args, instrPtr = new_loc }
            5  -> prog { instrPtr = if head args /= 0 then args!!1 else new_loc }
            6  -> prog { instrPtr = if head args == 0 then args!!1 else new_loc }
            7  -> prog { memory = mem // [(args!!2, if head args < args!!1 then 1 else 0)], instrPtr = new_loc }
            8  -> prog { memory = mem // [(args!!2, if head args == args!!1 then 1 else 0)], instrPtr = new_loc }
            9  -> prog { relativeBase = relativeBase prog + head args, instrPtr = new_loc }
            99 -> prog { isHalted = True }

makeAccessor :: Int -> (Program -> Int -> Int)
makeAccessor    0 = (!) . memory
makeAccessor    1 = const id
makeAccessor    2 = \prog loc -> memory prog ! (loc + relativeBase prog)
-- Internal nonsense workarounds
makeAccessor (-1) = \prog loc -> loc + relativeBase prog

-- Workaround mapping for types
fixAccess :: (Bool, Int) -> Int
fixAccess (False, a) = a
fixAccess (True,  0) = 1
fixAccess (True,  2) = -1

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
getOpInfo  9 = [False]
getOpInfo 99 = []

parseOpcode :: Program -> Instr
parseOpcode prog = Instr opCode access
   where rawData   = memory prog ! instrPtr prog
         opCode    = rawData `mod` 100
         getAccess = \divisor -> (rawData `div` divisor) `mod` 10
         isWrite   = getOpInfo opCode
         -- Apply workaround for weird addressing mode
         access    = take (length isWrite) $ makeAccessor . fixAccess <$> zip isWrite (getAccess <$> iterate (*10) 100)

runProgramInternal :: Program -> Program
runProgramInternal prog
   | isHalted prog = prog
   | otherwise     = runProgramInternal $ doOpcode (parseOpcode prog) prog

runProgram :: UArray Int Int -> [Int] -> Program
runProgram memory input = runProgramInternal $ Program memory input [] False 0 0

runUntilOutput :: Program -> Maybe (Program, Int)
runUntilOutput prog
   | isHalted prog      = Nothing
   | null $ output prog = runUntilOutput $ doOpcode (parseOpcode prog) prog
   | otherwise          = Just (prog { output = [] }, head $ output prog)

initProg :: UArray Int Int -> [Int] -> Program
initProg memory input = Program memory input [] False 0 0

-- We're just going to try and expand the memory a bunch to cheese out
-- needing a ton of memory
readProgramSize :: Int -> IO (UArray Int Int)
readProgramSize minLen = do
   input <- getContents
   let prog = read <$> splitBy (==',') (head $ lines input) :: [Int]
   let len = max minLen (length prog) - 1
   return $ array (0, len) (zip [0..] prog)

-- Default to 10k cells
readProgram :: IO (UArray Int Int)
readProgram = readProgramSize 10000

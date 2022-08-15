import Data.Array.IO ( IOUArray, newArray, newArray_, readArray, writeArray )
import Data.IORef ( IORef, readIORef, modifyIORef, writeIORef, newIORef )

-- This honestly would be better represented in a pure manner
-- Oh well, learned mutability from it

data Program = Program
   { opCodes      :: IOUArray Int Int
   , arguments    :: IOUArray Int Int
   , beenAccessed :: IOUArray Int Bool
   , accumulator  :: IORef Int
   , instrPtr     :: IORef Int
   , size         :: Int
   }

-- I'd like to do something like this, but I don't know enough to get it working with IOUArray
-- data Opcode = Acc | Jmp | Nop
-- readOpcode :: String -> Opcode
-- readOpcode "acc" = Acc
-- readOpcode "jmp" = Jmp
-- readOpcode "nop" = Nop

parseOpcode :: String -> Int
parseOpcode "acc" = 0
parseOpcode "jmp" = 1
parseOpcode "nop" = 2

parseArgument :: String -> Int
parseArgument ('+':rest) = read rest
parseArgument ('-':rest) = negate $ read rest

-- I would've liked to had this in the body of runInstruction, but newlines
-- can't be after "case x" so it would've meant everything was very off the right
applyOp :: Program -> Int -> Int -> IO ()
applyOp prog op arg
   | op == 0 = modifyIORef (accumulator prog) (+arg)
   | op == 1 = modifyIORef (instrPtr prog) (+(arg - 1))
   | op == 2 = return ()

runInstruction :: Program -> IO ()
runInstruction prog = do
   pc <- readIORef $ instrPtr prog
   op <- readArray ops pc
   arg <- readArray args pc
   writeArray (beenAccessed prog) pc True
   applyOp prog op arg
   modifyIORef (instrPtr prog) (+1)
      where ops  = opCodes prog
            args = arguments prog

-- Try to redo this with some fold later
makeProg :: Int -> IO Program
makeProg size = do
      ops <- makeArr
      args <- makeArr
      access <- newArray (0, size - 1) False
      acc <- makeRef
      ptr <- makeRef
      return $ Program ops args access acc ptr size
   where makeArr = newArray_ (0, size - 1)
         makeRef = newIORef (0 :: Int)

parseLine :: String -> (Int, Int)
parseLine input = (parseOpcode opStr, parseArgument $ tail argStr)
   where (opStr, argStr) = break (==' ') input

-- This feels so weird to do in Haskell... there's probably a better way?
-- Bool represents if it terminated
runUntilLoopOrEnd :: Program -> IO (Bool, Int)
runUntilLoopOrEnd prog = do
   iptr <- readIORef $ instrPtr prog
   hasBeenRun <- readArray (beenAccessed prog) iptr
   if iptr + 1 == size prog then
      do
         runInstruction prog
         val <- readIORef $ accumulator prog
         return (True, val)
   else if hasBeenRun then
      do
         val <- readIORef $ accumulator prog
         return (False, val)
   else
      do
         runInstruction prog
         runUntilLoopOrEnd prog

-- We're doing this by brute force I don't care
replaceProgramOutput :: Program -> Int -> IO (Bool, Int)
replaceProgramOutput prog swapLoc = do
   -- ignore requests where it's neither nop nor jmp
   oldOp <- readArray (opCodes prog) swapLoc
   if oldOp == 0 then return (False, -1)
   else do
      -- reset program state
      writeArray (opCodes prog) swapLoc $ mapInstr oldOp
      writeIORef (accumulator prog) 0
      writeIORef (instrPtr prog) 0
      -- I don't know a better way to do this...
      mapM_ (\x -> writeArray (beenAccessed prog) x False) [0..size prog - 1]
      -- This seems like it would do it but it returns a new array
      -- mapArray (const False) $ beenAccessed prog
      retVal <- runUntilLoopOrEnd prog
      writeArray (opCodes prog) swapLoc oldOp
      return retVal
   where mapInstr 1 = 2
         mapInstr 2 = 1

-- I'm like 100% sure there's a better way to do this, need to learn more monads
dumbSolvePart2 :: Program -> Int -> IO (Bool, Int)
dumbSolvePart2 prog loc = do
   result <- replaceProgramOutput prog loc
   if fst result then return result else dumbSolvePart2 prog $ loc + 1

main :: IO ()
main = do
   input <- getContents
   let rows = lines input
   prog <- makeProg $ length rows
   let ops = opCodes prog
   let args = arguments prog
   let writeData = (\(loc, (op, arg)) -> writeArray ops loc op >> writeArray args loc arg) :: (Int, (Int, Int)) -> IO ()
   mapM_ writeData $ zip [0..] $ parseLine <$> rows
   result <- runUntilLoopOrEnd prog
   putStr "Part 1: "
   print result
   putStr "Part 2: "
   result2 <- dumbSolvePart2 prog 0
   print $ snd result2


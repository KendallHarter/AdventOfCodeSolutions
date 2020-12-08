import Intcode ( runProgram, readProgram, output )

main :: IO ()
main = do
   prog <- readProgram
   putStr "Part 1: "
   print $ head $ output $ runProgram prog [1]
   putStr "Part 2: "
   print $ head $ output $ runProgram prog [2]

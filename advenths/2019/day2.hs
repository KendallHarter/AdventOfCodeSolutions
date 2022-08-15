import Data.Array ( Array, array, (!), (//) )
import Common ( splitBy )
import Data.Bifunctor ( second )

parseInstruction :: Array Int Int -> Int -> Maybe (Int, Int)
parseInstruction prog location
   | op ==  1 = Just (p3, p1 + p2)
   | op ==  2 = Just (p3, p1 * p2)
   | op == 99 = Nothing
   where op = prog!location
         p1 = prog!(prog!(location + 1))
         p2 = prog!(prog!(location + 2))
         p3 = prog!(location + 3)

runProgramInternal :: Int -> Array Int Int -> Array Int Int
runProgramInternal loc prog = maybe prog calc_next_state change
   where change = parseInstruction prog loc
         calc_next_state = \x -> runProgramInternal (loc + 4) (prog // [x])

runProgram :: Array Int Int -> Int
runProgram prog = runProgramInternal 0 prog ! 0

main :: IO ()
main = do
   input <- getContents
   let prog = read <$> splitBy (==',') (head $ lines input) :: [Int]
   let prog_array = array (0, length prog - 1) (zip [0..] prog)
   putStr "Part 1: "
   print $ runProgram $ prog_array // [(1, 12), (2, 2)]
   let replacements = [[(1, v1), (2, v2)] | v1 <- [0..99], v2 <- [0..99]]
   let all_progs = zip replacements ((prog_array //) <$> replacements)
   -- (second runProgram) is equivalent to (\(change, prog) -> (change, runProgram prog))
   let (offsets, _) = head $ filter (\(_, output) -> output==19690720) $ fmap (second runProgram) all_progs
   let noun = snd $ head offsets
   let verb = snd $ offsets!!1
   putStr "Part 2: "
   print $ 100 * noun + verb

import Intcode ( runProgram, readProgram, output, runUntilOutput, initProg, Program, input )
import Data.List ( foldl', permutations )
import Data.Array.Unboxed ( UArray )

calcOutput :: UArray Int Int -> [Int] -> Int
calcOutput prog inputs = head $ foldl' evalPartial [0] partialProgs
   where partialProgs = (\x -> output . runProgram prog . (x:)) <$> inputs
         evalPartial  = \prevOutput nextProg -> nextProg prevOutput

-- I'm sure there's a more generic and better way of doing this... but eh
-- Probably some kind of foldlM or something
doStep :: [Program] -> Maybe ([Program], Int)
doStep [a, b, c, d, e] = do
   let appendInput = \prog newIn -> prog { input = input prog ++ [newIn] }
   (new_a, a_out) <- runUntilOutput a
   (new_b, b_out) <- runUntilOutput $ appendInput b a_out
   (new_c, c_out) <- runUntilOutput $ appendInput c b_out
   (new_d, d_out) <- runUntilOutput $ appendInput d c_out
   (new_e, e_out) <- runUntilOutput $ appendInput e d_out
   return ([new_a, new_b, new_c, new_d, new_e], e_out)

calcOutputPart2Internal :: Int -> [Program] -> ([Program], Int)
calcOutputPart2Internal prev progs = case newValue of
      Nothing               -> ([], prev)
      Just (nextProgs, out) -> calcOutputPart2Internal out nextProgs
   where newValue     = doStep newProgs
         newProgs     = updatedFirst:tail progs
         firstProg    = head progs
         updatedFirst = firstProg { input = input firstProg ++ [prev] }

calcOutputPart2 :: Int -> [Program] -> Int
calcOutputPart2 init1Value progs = snd $ calcOutputPart2Internal init1Value progs

main :: IO ()
main = do
   prog <- readProgram
   putStr "Part 1: "
   print $ foldl' max minBound $ calcOutput prog <$> permutations [0..4]
   let makeProgs = fmap (initProg prog . (:[]))
   let calcProgs = \(val1:rest) -> calcOutputPart2 0 $ makeProgs (val1:rest)
   putStr "Part 2: "
   print $ foldl' max minBound $ calcProgs <$> permutations [5..9]

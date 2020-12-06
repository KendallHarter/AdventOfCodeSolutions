import Common ( splitBy, mapSelf )
import Data.List ( foldl', sort )

-- I'm nearly positive there's a better way to do this, but I can't think of it
andListsInternal :: [[Bool]] -> [Bool] -> [Bool]
andListsInternal rest soFar
   = foldl' (\soFar toAnd -> uncurry (&&) <$> zip soFar toAnd) soFar rest
-- andLists [] soFar = soFar
-- andLists (toAnd:rest) soFar = andLists rest $ uncurry (&&) <$> zip soFar toAnd

andLists :: [[Bool]] -> [Bool]
andLists input = andListsInternal input $ repeat True

main :: IO ()
main = do
   input <- getContents
   let answers = splitBy (==[]) (lines input)
   let sortedMergedAnswers = sort . foldl' (++) "" <$> answers
   let isSame = mapSelf (==) <$> sortedMergedAnswers
   -- Since one will always be dropped with isSame, need to add the number of inputs
   let numAnswers = (+) (length isSame) $ sum $ length . filter not <$> isSame
   putStr "Part 1: "
   print numAnswers
   let didAnswer = (\ans -> flip elem ans <$> ['a'..'z']) :: String -> [Bool]
   let didAnswerList = fmap didAnswer <$> answers
   let didAllAnswer = andLists <$> didAnswerList
   putStr "Part 2: "
   print $ sum $ length . filter id <$> didAllAnswer

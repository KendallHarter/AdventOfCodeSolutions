import Common ( splitBy, extractMaybe )
import Data.List ( foldl', sort, elemIndex )

solveOrbitsInternal :: [(String, String)] -> String -> Int -> [String]  -> (Int, [String], [String])
solveOrbitsInternal edges node count nodePath = (numObjsOrbiting, retYouPath, retSanPath)
   where objsOrbiting    = snd <$> filter ((==node) . fst) edges
         retYouPath      = if node == "YOU" then nodePath else getHeadOrEmpty $ getNextPath (\(_, x, _) -> x)
         retSanPath      = if node == "SAN" then nodePath else getHeadOrEmpty $ getNextPath (\(_, _, x) -> x)
         getNextPath     = \f -> filter (not . null) $ fmap f nextResults
         getHeadOrEmpty  = \x -> if null x then [] else head x
         numObjsOrbiting = foldl' (+) count $ fmap (\(x, _, _) -> x) nextResults
         nextResults     = (\x -> solveOrbitsInternal edges x (count + 1) (node:nodePath)) <$> objsOrbiting

solveOrbits :: [(String, String)] -> String -> (Int, [String], [String])
solveOrbits edges root = solveOrbitsInternal edges root 0 []

main :: IO ()
main = do
   input <- getContents
   let edgesBase = splitBy (==')') <$> lines input
   let edgeNamesAll = sort $ foldl' (++) [] edgesBase
   let dropName = False:(uncurry (==) <$> zip (tail edgeNamesAll) edgeNamesAll)
   let edgeNames = snd <$> filter (not . fst) (zip dropName edgeNamesAll)
   let edges = (\[x, y] -> (x, y)) <$> edgesBase
   let hasDependency = \x -> elem x $ fmap snd edges
   let root = snd $ head $ filter (not . fst) $ zip (hasDependency <$> edgeNames) edgeNames
   let (numOrbits, youPath, sanPath) = solveOrbits edges root
   putStr "Part 1: "
   print numOrbits
   -- Find first common parent for both paths and add number of step for both to it
   let firstShared = head $ filter (`elem` sanPath) youPath
   let pathIndex = extractMaybe . elemIndex firstShared
   putStr "Part 2: "
   print $ pathIndex youPath + pathIndex sanPath

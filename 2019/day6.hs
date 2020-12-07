import Common ( splitBy )
import Data.List ( foldl', sort )

-- This is correct, but far too slow and doesn't work well with the second part
-- Need to touch this up
countOrbit :: [(String, String)] -> String -> Int
countOrbit orbits obj = length objectsOrbited + foldl' (+) 0 (countOrbit orbits <$> objectsOrbited)
   where objectsOrbited = fst <$> filter ((==obj) . snd) orbits

main :: IO ()
main = do
   input <- getContents
   let edgesBase = splitBy (==')') <$> lines input
   let edgeNamesAll = sort $ foldl' (++) [] edgesBase
   let dropName = False:(uncurry (==) <$> zip (tail edgeNamesAll) edgeNamesAll)
   let edgeNames = snd <$> filter (not . fst) (zip dropName edgeNamesAll)
   let edges = (\[x, y] -> (x, y)) <$> edgesBase
   putStr "Part 1: "
   print $ foldl' (+) 0 $ countOrbit edges <$> edgeNames
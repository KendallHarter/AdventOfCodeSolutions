import Common ( splitBy, removeDuplicates, extractMaybe )
import Data.List ( foldl', find )
import Debug.Trace

data Rule = Rule
   { color    :: String
   , contents :: [(Int, String)]
   }

instance Show Rule where
   show (Rule color contents)
      = color ++ " bags contain " ++ show contents

-- This is probably super hacky but I don't care
parseBagColor :: [String] -> (String, [String])
parseBagColor (color1:color2:_:_:rest) = (color1 ++ color2, rest)

parseContains :: [String] -> [(Int, String)]
parseContains ("no":_) = []
parseContains [] = []
parseContains (num:color1:color2:_:rest) = (read num, color1 ++ color2):parseContains rest

parseLine :: String -> Rule
parseLine input = Rule color contains
   where (color, rest) = parseBagColor $ splitBy (==' ') input
         contains      = parseContains rest

hasBagColor :: String -> (Rule -> Bool)
hasBagColor color = or . fmap ((==color) . snd) <$> contents

containingColors :: String -> [Rule] -> [String]
containingColors findColor rules
   | null curContainColors = []
   | otherwise             = foldl' (++) curContainColors $ fmap countColor curContainColors
   where curContainColors = color <$> filter (hasBagColor findColor) rules
         countColor       = (`containingColors` rules)

shinyGoldHolders :: [Rule] -> [String]
shinyGoldHolders = containingColors "shinygold"

bagRule :: String -> [Rule] -> Rule
bagRule findColor = extractMaybe . find ((==findColor) . color)

containCount :: Rule -> [Rule] -> Int
containCount (Rule _ []) _ = 0
containCount (Rule _ contents) rules = foldl' (+) 0 $ bagCount <$> contents
   where bagCount = \(num, nextColor) -> num + num * containCount (bagRule nextColor rules) rules

main :: IO ()
main = do
   input <- getContents
   let rows = lines input
   let rules = parseLine <$> rows
   putStr "Part 1: "
   print $ length $ removeDuplicates $ shinyGoldHolders rules
   putStr "Part 2: "
   print $ containCount (bagRule "shinygold" rules) rules

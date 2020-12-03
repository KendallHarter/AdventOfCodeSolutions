import qualified System.Environment ( getArgs )

getPasswordStats :: String -> (Int, Int, Char)
getPasswordStats input = (min, max, char)
   where i num = read num :: Int
         (min_str, _:rest1) = break (=='-') input
         min = i min_str
         (max_str, _:rest2) = break (==' ') rest1
         max = i max_str
         char = head rest2

isValidPasswordPart1 :: String -> Bool
isValidPasswordPart1 input = num_chars >= min && num_chars <= max
   where num_chars = length $ filter (==char) password
         -- Ignore the colon and the space after the colon
         (stats, _:_:password) = break (==':') input
         (min, max, char) = getPasswordStats stats

xor :: Bool -> Bool -> Bool
a `xor` b = a /= b

isValidPasswordPart2 :: String -> Bool
isValidPasswordPart2 input = (password!!(pos1 - 1) == char) `xor` (password!!(pos2 - 1) == char)
   where (stats, _:_:password) = break (==':') input
         (pos1, pos2, char) = getPasswordStats stats

main :: IO ()
main = do
   -- System.Environment.getArgs >>= \x -> mapM_ print x
   input <- getContents
   putStr "Part 1: "
   print $ length $ filter isValidPasswordPart1 $ lines input
   putStr "Part 2: "
   print $ length $ filter isValidPasswordPart2 $ lines input

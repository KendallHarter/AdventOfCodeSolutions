import Data.List ( foldl1' )

splitLayersInternal :: (String, String) -> Int -> [String]
splitLayersInternal (a, []) _   = [a]
splitLayersInternal (a,  b) val = a:splitLayersInternal (splitAt val b) val

splitLayers :: String -> Int -> [String]
splitLayers input val = splitLayersInternal (splitAt val input) val

main :: IO ()
main = do
   input <- getContents
   let pic = head $ lines input
   let layers = splitLayers pic $ 25 * 6
   let layerNumZeros = zip layers $ length . filter (=='0') <$> layers
   -- I feel like there's a better way to express this, but I can't think of it
   let minZeros = fst $ foldl1' (\soFar rest -> if snd soFar < snd rest then soFar else rest) layerNumZeros
   let numOnes = length $ filter (=='1') minZeros
   let numTwos = length $ filter (=='2') minZeros
   putStr "Part 1: "
   print $ numOnes * numTwos
   -- I feel like there's a better way to do this...
   let pixelLayers = (\x -> (!!x) <$> layers) <$> [0..length (head layers) - 1]
   let colors = head . filter (/='2') <$> pixelLayers
   putStrLn "Part 2:"
   mapM_ (putStrLn . map (\x -> if x == '1' then 'X' else ' ')) $ splitLayers colors 25

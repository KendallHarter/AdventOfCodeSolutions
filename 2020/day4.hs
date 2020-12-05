import Common ( splitBy, between, extractMaybe )
import Data.Maybe ( isJust )
import Data.List ( elemIndex )

data Passport = Passport
   { birthYear  :: Int
   , issueYear  :: Int
   , expYear    :: Int
   , height     :: String
   , hairColor  :: String
   , eyeColor   :: String
   , passwordId :: String
   , countryId  :: Maybe Int
   }

makePair :: String -> (String, String)
makePair input = (head parsed, parsed !! 1)
   where parsed = splitBy (== ':') input

-- I feel there should be a better way to do this, but...
makePassport :: [String] -> Maybe Passport
makePassport input = do
   birthYear <- lookup "byr" pairs
   issueYear <- lookup "iyr" pairs
   expYear <- lookup "eyr" pairs
   height <- lookup "hgt" pairs
   hairColor <- lookup "hcl" pairs
   eyeColor <- lookup "ecl" pairs
   passwordId <- lookup "pid" pairs
   return $ Passport (read birthYear) (read issueYear) (read expYear) height hairColor eyeColor passwordId $ lookup "cid" pairs >>= \x -> Just $ read x
   where pairs = fmap makePair input

validHeightInner :: String -> Bool
validHeightInner ('n':'i':rest) = between (read $ reverse rest :: Int) 59 76
validHeightInner ('m':'c':rest) = between (read $ reverse rest :: Int) 150 193
validHeightInner _              = False

validHeight :: String -> Bool
validHeight input = validHeightInner $ reverse input

validHairColor :: String -> Bool
validHairColor ('#':rest) = length rest == 6 && all (\x -> between x '0' '9' || between x 'a' 'z') rest
validHairColor _ = False

validEyeColors :: [String]
validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPassport :: Passport -> Bool
isValidPassport pass
   =  between (birthYear pass) 1920 2002
   && between (issueYear pass) 2010 2020
   && between (expYear pass) 2020 2030
   && validHeight (height pass)
   && validHairColor (hairColor pass)
   && isJust (elemIndex (eyeColor pass) validEyeColors)
   && all (\x -> between x '0' '9') pid && length pid == 9
   where pid = passwordId pass

main :: IO ()
main = do
   input <- getContents
   let passports_raw = filter (/="") <$> fmap (splitBy (==' ') . foldr (\x y -> x ++ " " ++ y) "") (splitBy (==[]) $ lines input)
   let passports = fmap extractMaybe $ filter isJust $ fmap makePassport passports_raw
   putStr "Part 1: "
   print $ length passports
   putStr "Part 2: "
   print $ length $ filter isValidPassport passports

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.String.Utils
import Data.Either (rights)
import Data.List.Split (splitOn)
import System.Environment
import Text.Parsec.Char (crlf)

type Passport = [(String, String)]

tuplify2 :: [String] -> (String, String)
tuplify2 [x,y] = (x,y)
tuplify2 [] = ("", "")

-- passportLines = sepBy passportVars (char '\n')
passportVars = sepBy passportVar (oneOf " \n")
passportVar = fmap tuplify2 (sepBy (many1 (noneOf " \n:")) (char ':'))
-- parsePassportFileContent :: String -> Either ParseError [Passport]
-- parsePassportFileContent = parse passportLines "unknown"
-- parseCSV input = parse csvFile "(unknown)" input

parsePassportFromString :: String -> Either ParseError Passport
parsePassportFromString = parse passportVars "cannot parse vars"

passportStringsFromContent :: String -> [String]
passportStringsFromContent = splitOn "\n\n"

isPassportValid1 :: Passport -> Bool
isPassportValid1 pass =
    let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    in all (\field -> field `elem` map fst pass) requiredFields

valueBetween :: Int -> (Int, Int) -> Bool
valueBetween value (valueMin, valueMax) = value >= valueMin && value <= valueMax

isPassportFieldValid :: String -> String -> Bool
isPassportFieldValid "byr" value = valueBetween (read value) (1920, 2002)
isPassportFieldValid "iyr" value = valueBetween (read value) (2010, 2020)
isPassportFieldValid "eyr" value = valueBetween (read value) (2020, 2030)
isPassportFieldValid "hgt" value
    | endswith "cm" value = valueBetween (read $  replace "cm" "" value) (150, 193)
    | endswith "in" value = valueBetween (read $  replace "in" "" value) (59, 76)
    | otherwise = False
isPassportFieldValid "hcl" value = head value == '#' && all (`elem` "abcdef0123456789") (tail value)
isPassportFieldValid "ecl" value = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isPassportFieldValid "pid" value = (length value == 9) && all (`elem` "0123456789") value
isPassportFieldValid "cid" _     = True

checkPassportFields :: Passport -> Bool
checkPassportFields = all (uncurry isPassportFieldValid)

isPassportValid2 :: Passport -> Bool
isPassportValid2 pass =  isPassportValid1 pass && checkPassportFields pass


main :: IO ()
main = do
    content <- readFile "input_04a.txt"
    let passportLines = passportStringsFromContent content
        passports = rights $ map parsePassportFromString passportLines
    print $ sum $ map (fromEnum . isPassportValid1) passports
    print $ sum $ map (fromEnum . isPassportValid2) passports
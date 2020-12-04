import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List.Split (splitOn)
import System.Environment
import Text.Parsec.Char (crlf)

type Passport = [(String, String)]

tuplify2 :: [String] -> (String, String)
tuplify2 [x,y] = (x,y)
tuplify2 [] = ("", "")

-- passports = endBy many passportLines eol
-- passportLines = sepBy passportVars (char '\n')
passportVars = sepBy passportVar (oneOf " \n")
passportVar = fmap tuplify2 (sepBy (many1 (noneOf " \n:")) (char ':'))
-- eol ol = char '\n'

-- parsePassportFileContent :: String -> Either ParseError [Passport]
-- parsePassportFileContent = parse passportLines "unknown"
-- parseCSV input = parse csvFile "(unknown)" input

parsePassportFromString :: String -> Either ParseError Passport
parsePassportFromString = parse passportVars "cannot parse vars"


-- splitOn = split . dropDelims . onSublist

passportStringsFromContent :: String -> [String]
passportStringsFromContent = splitOn "\n\n" 

main :: IO ()
main = do
    content <- readFile "input_04a.txt"
    print $ parsePassportFileContent asdf
module Main where

import Data.Text (pack, unpack, splitOn)
import Data.List.Split (splitWhen)
import qualified Data.List.Split (splitOn)

data Input = Input {
    specs:: [String],
    my:: [String],
    others:: [String]
}

splitContent :: String -> Input
splitContent content = 
    let contents = map (lines . unpack) $ splitOn (pack "\n\n") (pack content)
    in Input (contents !! 0) (contents !! 1) (contents !! 2)

data Range = Range Int Int deriving (Show)
data Rule = Rule {
    ruleKey :: String,
    ruleRanges :: [Range]
} deriving (Show)


parseRangeFromString :: String -> Range
parseRangeFromString s = 
    let (mi:ma:_) = Data.List.Split.splitOn "-" s
    in Range (read mi) (read ma)


parseRuleFromString :: String -> Rule
-- train: 45-798 or 813-954
parseRuleFromString s = 
    let lr = Data.List.Split.splitOn ": " s
        key = head lr
        rangesPart = head $ tail lr
        ranges = map parseRangeFromString $ Data.List.Split.splitOn " or " rangesPart
    in Rule key ranges

parseRules :: [String] -> [Rule]
parseRules = map parseRuleFromString 

type Ticket = [Int]

inRange :: Int -> Range -> Bool
inRange value (Range mi ma) = 
    mi <= value && value <= ma

parseTicket :: String -> Ticket
parseTicket s = map read $ Data.List.Split.splitOn "," s

valueNotInRules :: [Rule] -> Int -> [Int]
valueNotInRules rules value = 
    let ranges = concatMap ruleRanges rules
        inRuleList = do 
            range <- ranges
            return $ inRange value range
    in if any (True==) inRuleList 
        then [] else [value]

getInvalidValue :: [Rule] -> Ticket -> [Int]
getInvalidValue rules ticket = 
    concatMap (valueNotInRules rules) ticket

isTicketValid :: [Rule] -> Ticket -> Bool
isTicketValid rules tickets = not $ null $ getInvalidValue rules tickets

getInvalidValues :: [Rule] -> [Ticket] -> [Int]
getInvalidValues rules tickets = concat $ map (getInvalidValue rules) tickets

solve1 :: [Rule] -> [Ticket] -> Int
solve1 rules tickets = 
    sum $ getInvalidValues rules tickets

main :: IO ()
main = do
   content <- readFile "input_16a.txt"
   let contents = splitContent content 
       rules = parseRules $ specs contents
       myTicket = parseTicket $ head $ tail $ my contents
       otherTickets = map parseTicket $ tail $ others contents
       result1 = solve1 rules otherTickets
       validTickets = [ticket | ticket <- otherTickets, isTicketValid rules ticket]
   print result1
--    print $ rules
--    print $ myTicket
--    print $ otherTickets


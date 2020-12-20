module Main where

import Debug.Trace
import Data.List (findIndex, sort)
import Data.Maybe (fromJust)
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

isValueInAnyRule :: [Rule] -> Int -> Bool
isValueInAnyRule rules value = 
    let ranges = concatMap ruleRanges rules
        inRuleList = do 
            range <- ranges
            return $ inRange value range
    in any (True==) inRuleList 

getInvalidValue :: [Rule] -> Ticket -> [Int]
getInvalidValue rules ticket = 
    concatMap (valueNotInRules rules) ticket

isTicketValid :: [Rule] -> Ticket -> Bool
-- isTicketValid rules ticket = any (null) $ getInvalidValue rules ticket
isTicketValid rules ticket = all (==True) $ map (isValueInAnyRule rules) ticket

getInvalidValues :: [Rule] -> [Ticket] -> [Int]
getInvalidValues rules tickets = concat $ map (getInvalidValue rules) tickets

solve1 :: [Rule] -> [Ticket] -> Int
solve1 rules tickets = 
    sum $ getInvalidValues rules tickets

-- newtype ZipList a = ZipList { getZipList :: [a] }

-- instance Applicative ZipList where
--     pure x = ZipList (repeat x)
--     ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- transpose :: [[a]] -> [[a]]
-- transpose = getZipList . traverse ZipList

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

columnMatchRule' values (Rule _ ranges) | trace ("columnMatchRule " ++ show values ++ " " ++ show ranges) False = undefined
columnMatchRule' values (Rule _ ranges) = 
    do
    value <- values
    return $ any (==True) $ map (inRange value) ranges

columnMatchRule ::  [Int] -> Rule -> Bool
columnMatchRule values (Rule _ ranges) = 
    all (==True) $ do 
    value <- values
    return $ any (==True) $ map (inRange value) ranges

enumerate :: [a] -> [(Int, a)]
enumerate l = zip [0..length l] l

distribute :: [[Int]] -> [[Int]]
distribute [solutions] = fmap pure solutions
distribute (x:xs) = 
    let newSolutions = do
            solution <- distribute xs
            this <- x 
            if this `elem` solution then return []
            else [this:solution]
    in filter (not . null) newSolutions

searchMatchingRule :: [Rule] -> [Int] -> [Int]
searchMatchingRule rules column = 
    let matchingRules = map (columnMatchRule column) rules
    in map fst $ filter  ( (==True) . snd) $ enumerate matchingRules

startsWith :: String -> String -> Bool
startsWith query value =
    all (==True) $ zipWith (==) query value

-- solve2 :: [Rule] -> [Ticket] -> Ticket -> Int
solve2 rules tickets myTicket = 
    let columns = transpose tickets 
        matchingRules = map (searchMatchingRule rules) columns
        distribution = head $ distribute matchingRules
        ruleWithOrder = zip rules distribution
    in product [ myTicket !! index | (key, index) <- ruleWithOrder, startsWith "departure" (ruleKey key) ] 
    -- in ruleWithOrder


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
   let result2 = solve2 rules validTickets myTicket
    --    columns = transpose validTickets
--    print $ columns !! 0
--    print $ columnMatchRule' (columns !! 0) (rules !! 1)
   print $ result2
--    print $ Data.List.sort [3,9,18,8,0,4,14,15,1,13,7,5,17,16,19,6,11,10,2,12] 
--    print result2
--    print $ rules
--    print $ myTicket
--    print $ otherTickets


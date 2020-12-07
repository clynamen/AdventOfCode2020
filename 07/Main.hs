import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.String.Utils
import Data.Either (rights)
import Data.List.Unique (sortUniq)
import Data.List.Split (splitOn)
import System.Environment
import Text.Parsec.Char (crlf)

exampleLine = "dull coral bags contain 1 dim olive bag, 5 muted violet bags, 2 dark gray bags."

newtype Pattern = Pattern String deriving (Eq, Show, Ord)
newtype Color = Color String deriving (Eq, Show, Ord)

data Bag = Bag Pattern Color deriving (Eq, Show, Ord)

data Statement = Statement Bag [(Int, Bag)] deriving (Eq, Show, Ord)

data WDag =  WDagElem Bag [(Int, WDag)] | WDagLeaf Bag


-- tryAddConnection :: WDag -> Bag -> Int -> Bag -> WDag
-- tryAddConnection (WDagLeaf frm) weight to = WDagElem frm [(weight, WDagLeaf to)]
-- tryAddConnection (WDagElem frm (x:xs)) weight to =
--     WDagElem frm $ map tryAddConnection

getParentInStatement :: Bag -> Statement -> [Bag]
getParentInStatement query (Statement parent children) = do
    (cnt, child) <- children
    [parent | query == child]

searchParents :: Bag -> [Statement] -> [Bag]
searchParents query statements =
    let parents = concatMap (getParentInStatement query) statements
        ancestors = concat $ [searchParents parent statements |
            parent <- parents]
    in parents ++ ancestors

getChildren :: Bag -> [Statement] -> [(Int, Bag)]
getChildren query statements = concat $ [children | Statement parent children <- statements, query == parent]

getDescendantsCount :: Bag -> [Statement] -> [Int]
getDescendantsCount query statements = do
    (cnt, child) <- getChildren query statements
    cnt : (map (cnt*) $ getDescendantsCount child statements)


parseBag :: GenParser Char st Bag
parseBag = do
    spaces
    pat <- many $ noneOf " "
    spaces
    col <- many $ noneOf " "
    spaces
    string "bag"
    optional $ char 's'
    return $ Bag (Pattern pat) (Color col)

parseBagContents :: GenParser Char st [(Int, Bag)]
parseBagContents = sepBy parseBagContent (oneOf ",")


parseBagContent :: GenParser Char str (Int, Bag)
parseBagContent = do
    spaces
    countString <- many1 digit
    spaces
    bag <- parseBag
    return (read countString :: Int, bag)


parseStatementLine :: GenParser Char st Statement
parseStatementLine = do
    bag <- parseBag
    spaces
    string "contain"
    spaces
    Statement bag <$> parseBagContents

parseStatement :: String -> Either ParseError Statement
parseStatement = parse parseStatementLine "cannot parse statement"


main :: IO ()
main = do
    content <- readFile "input_07a.txt"
    let statements = rights $ map parseStatement $ lines content
    print "Part 1"
    print $ length $ sortUniq $ searchParents (Bag (Pattern "shiny") (Color "gold")) statements
    print "Part 2"
    print $ sum $ getDescendantsCount (Bag (Pattern "shiny") (Color "gold")) statements
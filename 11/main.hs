import Data.Maybe (fromJust)
import Debug.Trace

data Map = Map {
    width :: Int,
    height :: Int,
    dat :: String
} deriving (Eq)

type Vec2 = (Int, Int)

getAt :: Vec2 -> Map -> Maybe Char
getAt (x, y) m
    | x < 0 || x >= width m = Nothing
    | y < 0 || y >= height m = Nothing
    | otherwise = Just $ dat m !! (y * width m + x)


readMapFromFile :: String -> IO Map
readMapFromFile fname = do
    content <- readFile fname
    let nRows = length $ lines content
        nCols = length $ head $ lines content
    return $ Map nCols nRows [c | c <- content , c /= '\n']

indexToVec :: Int -> Map -> Vec2
indexToVec index m =
    let row = index `div` width m
        col = index - row * width m
    in (col, row)


countOccupiedNeighborhood :: Vec2 -> Map -> Int
countOccupiedNeighborhood (cx, cy) m = sum $ concat $ do
    x <- [-1..1]
    y <- [-1..1]
    return $ if x == 0 && y == 0 then
            [0]
        else
            [fromEnum $ getAt (cx+x, cy+y) m == Just '#']

countOccupiedNeighborhood' :: Vec2 -> Map -> [Vec2]
countOccupiedNeighborhood' (cx, cy) m = concat $ do
    x <- [-1..1]
    y <- [-1..1]
    return [(cx+x, cy+y)]


type NextStateFun = Vec2 -> Char -> Map -> Char


nextStateFun1 :: NextStateFun
nextStateFun1 vec currentState m =
    let occupied = countOccupiedNeighborhood vec m
    in case currentState of
        'L' | occupied == 0 -> '#'
        '#' | occupied >= 4 -> 'L'
        _ -> currentState

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (ax, ay) (bx, by) = (ax+bx, ay+by)

searchFirstSeatInDirection :: Vec2 -> Vec2 -> Map -> Char
searchFirstSeatInDirection start dir m =
    let nextPos = start `addVec2` dir
    in  case getAt nextPos m of
            Nothing -> '.'
            Just 'L' -> 'L'
            Just '#' -> '#'
            Just '.' -> searchFirstSeatInDirection nextPos dir m

countOccupiedNeighborhood2 :: Vec2 -> Map -> Int 
countOccupiedNeighborhood2 (cx, cy) m =
    sum $ concat $ do
        x <- [-1..1]
        y <- [-1..1]
        return $ if x == 0 && y == 0 then
                [0]
            else
                [fromEnum $ searchFirstSeatInDirection (cx, cy) (x, y) m == '#']

nextStateFun2 :: NextStateFun
nextStateFun2 vec currentState m = 
    let occupied = countOccupiedNeighborhood2 vec m
    in case currentState of
        'L' | occupied == 0 -> '#'
        '#' | occupied >= 5 -> 'L'
        _ -> currentState


nextValue :: NextStateFun -> Map -> Int -> Char
nextValue nextStateFun m index = let
    vec =  indexToVec index m
    currentState = fromJust $  getAt vec m
    in nextStateFun vec currentState m


step :: NextStateFun -> Map -> Map
step nextStateFun m = Map (width m) (height m) $ map (nextValue nextStateFun m) [0..(length $ dat m) - 1]

simulateUntilConstant :: NextStateFun -> Map -> Map
simulateUntilConstant nextStateFun m = let
    newState = step nextStateFun m
    in if newState == m then
        m
    else
        simulateUntilConstant nextStateFun newState

subsetOfN :: Int -> String -> [String]
subsetOfN _ [] = []
subsetOfN n xs = take n xs : subsetOfN n (drop n xs)

mapRows :: Map -> [String]
mapRows m = subsetOfN (width m) (dat m)

printMap :: Map -> IO ()
printMap m = do
    putStrLn ""
    mapM_ (putStrLn) $ mapRows m
    putStrLn ""

count :: Eq a => (a -> Bool) -> [a] -> Int
count fun = sum . map (fromEnum . fun)

countOccupied :: Map -> Int
countOccupied m = count (=='#') $ dat m

main :: IO ()
main = do
    m <- readMapFromFile "input_11a.txt"
    -- printMap m
    -- print $ countOccupiedNeighborhood (2, 0) m
    -- printMap $ step m
    -- print $ countOccupiedNeighborhood' (indexToVec 18 m) m
    -- print $ map (`getAt` m) $ countOccupiedNeighborhood' (indexToVec 18 m) m
    -- print $ countOccupiedNeighborhood (indexToVec 18 m) m
    -- printMap $ step $ step m
    -- print $ [-1..1]
    let m1 = simulateUntilConstant nextStateFun1 m
    print $ countOccupied m1
    print $ countOccupied $ simulateUntilConstant nextStateFun2 m



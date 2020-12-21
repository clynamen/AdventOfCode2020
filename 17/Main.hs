module Main where

import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

data Vec4 = Vec4 {
    vecX :: Int,
    vecY :: Int,
    vecZ :: Int,
    vecW :: Int
} deriving (Show, Eq)


instance Ord Vec4 where  
    a <= b = vecToList a <= vecToList b


vecToList :: Vec4 -> [Int]
vecToList vec = [vecX vec, vecY vec, vecZ vec, vecW vec]

listToVec :: [Int] -> Vec4
listToVec list = Vec4 (list !! 0) (list !! 1) (list !! 2) (list !! 3)

addVec :: Vec4 -> Vec4 -> Vec4
addVec a b = listToVec $ zipWith (+) (vecToList a) (vecToList b)

neighbours :: Vec4 -> [Vec4]
neighbours origin = do
    dx <- [-1..1]
    dy <- [-1..1]
    dz <- [-1..1]
    if dx == 0 && dy == 0 && dz == 0 then []
    else [origin `addVec` Vec4 dx dy dz 0] 

neighbours4 :: Vec4 -> [Vec4]
neighbours4 origin = do
    dx <- [-1..1]
    dy <- [-1..1]
    dz <- [-1..1]
    dw <- [-1..1]
    if dx == 0 && dy == 0 && dz == 0 && dw == 0 then []
    else [origin `addVec` Vec4 dx dy dz dw] 

type Map3d = Map Vec4 Char

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..length xs] xs

mapGetAt :: Map3d -> Vec4 -> Char
mapGetAt m v = 
    case Map.lookup v m of 
        Just c -> c
        Nothing -> '.'

count :: (Eq a) => (a -> Bool) -> [a] -> Int
count pred xs = length [x | x <- xs, pred x]

type NeighborhoodFunction = Vec4 -> [Vec4]

countActiveNeightborhood :: NeighborhoodFunction -> Map3d -> Vec4 -> Int
countActiveNeightborhood nf m origin = count (=='#') $ map (mapGetAt m) $ nf origin

nextValue :: NeighborhoodFunction -> Map3d -> Vec4 -> Char -> Map3d
nextValue nf m origin c = 
    let activeNeighborhoodCount = countActiveNeightborhood nf m origin
        newC = case c of 
            '#' | activeNeighborhoodCount == 2 || activeNeighborhoodCount == 3 -> '#'
            '.' | activeNeighborhoodCount == 3 -> '#'
            otherwise  -> '.'
    in Map.singleton origin newC

addInactiveNeighborhood :: NeighborhoodFunction -> Map3d -> Map3d
addInactiveNeighborhood nf m = m `Map.union` (Map.fromList $ zip (concat $ map nf (Map.keys m )) (repeat '.') )

getNextValueAndJoin :: NeighborhoodFunction -> Map3d -> Vec4 -> Char -> Map3d -> Map3d
-- getNextValueAndJoin m origin c newMap | trace ("getNextValueAndJoin" ++ show origin ++ " newMap= " ++ show newMap ++ "\n") False = undefined
getNextValueAndJoin nf m origin c newMap = Map.union (nextValue nf m origin c) newMap 

nextCycle :: NeighborhoodFunction -> Map3d -> Map3d
nextCycle nf m = Map.foldrWithKey (getNextValueAndJoin nf m) Map.empty (addInactiveNeighborhood nf m)

runNCycles :: NeighborhoodFunction -> Int -> Map3d -> Map3d
runNCycles nf 0 m = m 
runNCycles nf n m = nextCycle nf $ runNCycles nf (n-1) m

parseInitialMap :: [[Char]] -> Map3d
parseInitialMap rows = 
    Map.fromList $ do
        (dy, row) <- enumerate rows
        (dx, char) <- enumerate row
        [(Vec4 dx dy 0 0, char)]

countActiveInMap :: Map3d -> Int
countActiveInMap m = Map.foldr (\x -> (+) (fromEnum $ x=='#') ) 0 m


print2DMap' :: [[Char]] -> IO ()
print2DMap' [] = return ()
print2DMap' (x:xs) = do
    putStrLn x 
    print2DMap' xs

print2DMap :: [[Char]] -> IO ()
print2DMap xs = do 
    putStrLn ""
    print2DMap' xs
    putStrLn ""

printMap :: Map3d -> IO ()
printMap m = 
    let vecs = 
            let xs = [-10..10]
                ys = [-10..10]
                zs = [-1..1] 
            in  [[[mapGetAt m (Vec4 x y z 0) | x <- xs] | y <- ys] | z <- zs] 
    in mapM_ print2DMap vecs
    

main :: IO ()
main = do
    fileRows <- lines <$> readFile "input_17a.txt"
    putStrLn "first part"
    let firstLayer = parseInitialMap fileRows
        lastCycleResult = runNCycles neighbours 6 firstLayer
    print $ countActiveInMap lastCycleResult
    putStrLn "\nsecond part"
    print $ countActiveInMap $ runNCycles neighbours4 6 firstLayer

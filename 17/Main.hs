module Main where

import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

data Vec3 = Vec3 {
    vecX :: Int,
    vecY :: Int,
    vecZ :: Int
} deriving (Show, Eq)

vecToKey :: Vec3 -> Int 
vecToKey (Vec3 ax ay az) = (ax*1000000 + ay * 1000 + az) 

instance Ord Vec3 where  
    a <= b = vecToKey a <= vecToKey b


vecToList :: Vec3 -> [Int]
vecToList vec = [vecX vec, vecY vec, vecZ vec]

listToVec :: [Int] -> Vec3
listToVec list = Vec3 (list !! 0) (list !! 1) (list !! 2)

addVec :: Vec3 -> Vec3 -> Vec3
addVec a b = listToVec $ zipWith (+) (vecToList a) (vecToList b)

neighbours :: Vec3 -> [Vec3]
neighbours origin = do
    dx <- [-1..1]
    dy <- [-1..1]
    dz <- [-1..1]
    if dx == 0 && dy == 0 && dz == 0 then []
    else [origin `addVec` Vec3 dx dy dz] 

type Map3d = Map Vec3 Char

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..length xs] xs

mapGetAt :: Map3d -> Vec3 -> Char
mapGetAt m v = 
    case Map.lookup v m of 
        Just c -> c
        Nothing -> '.'

count :: (Eq a) => (a -> Bool) -> [a] -> Int
count pred xs = length [x | x <- xs, pred x]

countActiveNeightborhood :: Map3d -> Vec3 -> Int
countActiveNeightborhood m origin = count (=='#') $ map (mapGetAt m) $ neighbours origin

nextValue :: Map3d -> Vec3 -> Char -> Map3d
nextValue m origin c = 
    let activeNeighborhoodCount = countActiveNeightborhood m origin
        newC = case c of 
            '#' | activeNeighborhoodCount == 2 || activeNeighborhoodCount == 3 -> '#'
            '.' | activeNeighborhoodCount == 3 -> '#'
            otherwise  -> '.'
    in Map.singleton origin newC

addInactiveNeighborhood :: Map3d -> Map3d
addInactiveNeighborhood m = m `Map.union` (Map.fromList $ zip (concat $ map neighbours (Map.keys m )) (repeat '.') )

getNextValueAndJoin :: Map3d -> Vec3 -> Char -> Map3d -> Map3d
-- getNextValueAndJoin m origin c newMap | trace ("getNextValueAndJoin" ++ show origin ++ " newMap= " ++ show newMap ++ "\n") False = undefined
getNextValueAndJoin m origin c newMap = Map.union (nextValue m origin c) newMap 

nextCycle :: Map3d -> Map3d
nextCycle m = Map.foldrWithKey (getNextValueAndJoin m) Map.empty (addInactiveNeighborhood m)

runNCycles :: Int -> Map3d -> Map3d
runNCycles 0 m = m 
runNCycles n m = nextCycle $ runNCycles (n-1) m

parseInitialMap :: [[Char]] -> Map3d
parseInitialMap rows = 
    Map.fromList $ do
        (dy, row) <- enumerate rows
        (dx, char) <- enumerate row
        [(Vec3 dx dy 0, char)]

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
            in  [[[mapGetAt m (Vec3 x y z) | x <- xs] | y <- ys] | z <- zs] 
    in mapM_ (print2DMap) vecs
    

main :: IO ()
main = do
    fileRows <- lines <$> readFile "input_17a.txt"
    let firstLayer = parseInitialMap fileRows
        lastCycleResult = runNCycles 6 firstLayer
        testOrigin = Vec3 2 2 (-1)
    putStrLn ""
    print $ nextValue firstLayer testOrigin (mapGetAt firstLayer testOrigin) 
    print $ countActiveInMap lastCycleResult

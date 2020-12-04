import Data.Array
import Prelude

data Forest = Forest {
    forestWidth :: Int,
    forestHeight :: Int,
    forestData :: ForestData
} deriving (Show)

type ForestData = Array Int Char

type Vec2 = (Int, Int)

addvec :: Vec2 -> Vec2 -> Vec2
addvec (ax, ay) (bx, by) = (ax+bx, ay+by)


linesToForest :: [String] -> Forest
linesToForest lines_ =
    let width = length $ head lines_
        height = length lines_
        data_ = Data.Array.listArray (0, width*height) $ concat lines_
    in Forest width height data_

getForestXY :: Forest -> Vec2 -> Char
getForestXY forest (x, y) =
    let x' = x `mod` forestWidth forest
        index = y * forestWidth forest + x'
    in forestData forest ! index

getForestFromFile :: String -> IO Forest
getForestFromFile fname = do
    content <- readFile fname
    let lines_ = lines content
    return $ linesToForest lines_


traverseForestWithXYSlope' :: Vec2 -> Forest -> Vec2 -> [Char]
traverseForestWithXYSlope' cur forest slope
    | snd cur >= forestHeight forest = []
    | otherwise =
        let nextPoint = addvec cur slope
        in getForestXY forest cur : traverseForestWithXYSlope' nextPoint forest slope

traverseForestWithXYSlope  :: Forest -> Vec2 -> [Char]
traverseForestWithXYSlope = traverseForestWithXYSlope' (0, 0)

boolToInt False = 0
boolToInt True = 1

countTree :: [Char] -> Int
countTree = sum . map (boolToInt . (=='#') )

main :: IO ()
main = do
    forest <- getForestFromFile "input_03a.txt"
    -- first solution
    print $ countTree $ traverseForestWithXYSlope forest (3, 1)
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    -- given a slope, evaluate the result by traversing the forest, count the tree and multiply
    -- the result
    let getResultForSlopeAndMultiply = (*) . (countTree . traverseForestWithXYSlope forest)
    -- second solution
    print $ foldr getResultForSlopeAndMultiply 1 slopes

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)


findChain' :: [Int] -> [Int]
findChain' = id

findChain :: [Int] -> [Int]
findChain numbers =
    let sorted = sort numbers
    in findChain' sorted


type Hist = Map Int Int

emptyHist :: Hist
emptyHist = Map.fromList $ zip [1..3] (repeat 0)

increaseHist :: Hist -> Int -> Hist
increaseHist map key =
    let value = (Map.!) map key
    in Map.insert key (value+1) map

hist :: [Int] -> Hist
hist = foldl increaseHist emptyHist

diffChain :: [Int] -> [Int]
diffChain [_] = []
diffChain (a:b:xs) = (b-a) : diffChain (b:xs)

solve1 :: [Int] -> Int
solve1 numbers =
    let chain = findChain numbers
        diffs = diffChain chain
        dist = hist diffs
    in (Map.!) dist 1 * (Map.!) dist 3

fullAdapterList :: [Int] -> [Int]
fullAdapterList list = 0:maximum list + 3:list  

main :: IO ()
main = do
    content <- readFile "input_10a.txt"
    let numbers = map (read :: String -> Int) $ lines content
    print $ solve1 $ fullAdapterList numbers

import Data.Map (Map)
import Debug.Trace
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


countSolve2 :: Int -> Int -> Int -> [Int] -> IO Int
countSolve2 cnt last index numbers
    | index == length numbers =
        if last == numbers !! ((length numbers) - 1) then do
            putStrLn $ (show last) ++ " stop"
            return 1
        else
            return 0
    | index < length numbers = do
        let
            next = (numbers !! index)
            other = countSolve2 (cnt+1) last (index+2) numbers
            diff = next - last in
                if diff < 4 then do
                    putStrLn $ replicate cnt '-' ++ show next
                    firstB <- countSolve2 (cnt+1) next (index+1) numbers
                    firstA <- other
                    return $ firstA + firstB
                else return 0
    | otherwise = return 0


solve2 :: [Int] -> IO Int
solve2 numbers = do
    let sorted = sort numbers
    putStrLn $ show sorted
    countSolve2 0 0 1 sorted

main :: IO ()
main = do
    content <- readFile "input_10a.txt"
    let numbers = map (read :: String -> Int) $ lines content
    print $ solve1 $ fullAdapterList numbers
    res2 <- solve2 $ fullAdapterList numbers
    print $ res2

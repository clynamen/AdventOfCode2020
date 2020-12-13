import Text.Printf

isInSet :: (Eq a) => a -> [a] -> Bool
isInSet v [] = False
isInSet v (x:xs)
    | v == x = True
    | otherwise = isInSet v xs

isSumOfPairInSet :: Int -> [Int] -> Bool
isSumOfPairInSet query [] = False
isSumOfPairInSet query (a:xs) =
    let b = (query-a) in 
        if isInSet b xs then
            True
        else isSumOfPairInSet query xs

getRange :: Int -> Int -> [Int] -> [Int]
getRange left right list
    | left >= right = []
    | left < right = (list !! left) : getRange (left+1) right list


solve1' :: Int -> Int -> [Int] -> Int
solve1' start windowSize numbers =
    let end = start+windowSize
        subset = getRange start (start+windowSize) numbers
        query = numbers !! end
    in if isSumOfPairInSet query subset then
           solve1' (start+1) windowSize numbers
       else
           query

solve1 :: [Int] -> Int
solve1 = solve1' 0 25

solve2' :: Int -> Int -> Int -> [Int] -> (Int, Int)
solve2' start len query numbers =
    let range = getRange start (len+1) numbers
        total = sum range
    in case () of
        _ | total == query -> (minimum range, maximum range)
        _ | total > query -> solve2' (start+1) 1 query numbers
        _ | total < query -> solve2' start (len+1) query numbers

solve2 :: Int -> [Int] -> (Int, Int)
solve2 = solve2' 0 1 

main :: IO ()
main = do
    inputText <-readFile "input_09a.txt"
    let numbers = map (read :: String -> Int) $ lines inputText
        result1 = solve1 numbers
        (result2a, result2b) = solve2 result1 numbers
    print $ result1
    print $ (printf "%d+%d = %d" result2a result2b (result2a+result2b) :: String)
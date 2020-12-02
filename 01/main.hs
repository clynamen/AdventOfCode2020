import Prelude

splitOn :: Eq c => c -> [c] -> [[c]]
splitOn on list = case list of
    [x]    -> [[x]]
    (x:xs) -> let (a:as) = splitOn on xs
            in case () of
                () | x == on -> []:a:as
                   | otherwise -> (x:a):as

getInput :: String -> IO [Int]
getInput fname = do
    fileContent <- readFile fname
    let lines = splitOn '\n' fileContent
    return $ map (read :: String -> Int) lines

getTwoNumberWithSumEqualTo :: [Int] -> Int -> (Int, Int)
getTwoNumberWithSumEqualTo list query =
    head [(i, j) | i <- list, j <- list, (i+j) == query]

getThreeNumberWithSumEqualTo :: [Int] -> Int -> (Int, Int, Int)
getThreeNumberWithSumEqualTo list query =
    head [(i, j, k) | i <- list, j <- list, k <- list, (i+j+k) == query]

main :: IO ()
main = do
    input <- getInput "input_01a.txt"
    let (i, j) = getTwoNumberWithSumEqualTo input 2020
    putStrLn $ "i*j = " ++ show (i*j)
    let (a, b, c) = getThreeNumberWithSumEqualTo input 2020
    putStrLn $ "a*b*c = " ++ show (a*b*c)
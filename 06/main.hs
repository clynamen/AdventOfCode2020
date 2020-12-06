import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type GroupAnswers = [String]
type CombineSetFunction =  Set Char -> Set Char -> Set Char

countGroupYesAnswers :: CombineSetFunction -> GroupAnswers -> Int
countGroupYesAnswers combineFunction answers =
    let startSet = Set.fromList $ head answers
        allAnswers = foldl (\l r -> l `combineFunction` Set.fromList r) startSet answers
    in Set.size allAnswers

main :: IO ()
main = do
    content <- readFile "input_06a.txt"
    let groupAnswers = map lines $ splitOn "\n\n" content
    print $ sum $ map (countGroupYesAnswers Set.union) groupAnswers
    print $ sum $ map (countGroupYesAnswers Set.intersection) groupAnswers
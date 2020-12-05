import Data.Set (Set)
import qualified Data.Set as Set

data Bin2 = Bin2 {
   binMiddle :: (Int, Int),
   binSize   :: (Int, Int)
} deriving (Show)

startBin2 :: Bin2
startBin2 = Bin2 (64, 4) (64, 4)


splitBin2 :: Bin2 -> Char -> Bin2
splitBin2 (Bin2 (y, x) (1, w)) 'F' = Bin2 (y - 1, x) (0, w)
splitBin2 (Bin2 (y, x) (1, w)) 'B' = Bin2 (y    , x) (0, w)
splitBin2 (Bin2 (y, x) (h, w)) 'F' = Bin2 (y - h `div` 2, x) (h `div` 2, w)
splitBin2 (Bin2 (y, x) (h, w)) 'B' = Bin2 (y + h `div` 2, x) (h `div` 2, w)
splitBin2 (Bin2 (y, x) (h, 1)) 'L' = Bin2 (y, x - 1) (h, 0)
splitBin2 (Bin2 (y, x) (h, 1)) 'R' = Bin2 (y, x    ) (h, 0)
splitBin2 (Bin2 (y, x) (h, w)) 'L' = Bin2 (y, x - w `div` 2) (h, w `div` 2)
splitBin2 (Bin2 (y, x) (h, w)) 'R' = Bin2 (y, x + w `div` 2) (h, w `div` 2)

findSeat' :: String -> Bin2
findSeat' = foldl splitBin2 startBin2

findSeat :: String -> (Int, Int)
findSeat = binMiddle . findSeat'

seatId :: (Int, Int) -> Int
seatId (y, x) = y * 8 + x

readBoardingPassessFromFile :: String -> IO [String]
readBoardingPassessFromFile fname = do
    content <- readFile fname
    return $ lines content

allAirplaneSeats :: Set (Int, Int)
allAirplaneSeats = Set.fromList [(y, x) | y <- [0..127], x <- [0..7]]



findSeatsWithNonEmptyNeighborhood :: Set (Int, Int) -> [(Int, Int)]
findSeatsWithNonEmptyNeighborhood remainingSeats =
    let seatIds = Set.map seatId remainingSeats
        neighborIsOccupied = \seat -> 
            not (Set.member (seatId seat - 1) seatIds) &&
            not (Set.member (seatId seat + 1) seatIds)
        result =  Set.filter neighborIsOccupied remainingSeats
    in Set.toList result

main :: IO ()
main = do
    print $ seatId $ findSeat "FFFBBBFRRR"
    passes <- readBoardingPassessFromFile "input_05a.txt"
    let seats = map findSeat passes
        maxSeat = maximum $ map seatId seats
    print maxSeat
    let remainingSeats = foldr Set.delete allAirplaneSeats seats 
        seatsWithNonEmptyNeighborhood = findSeatsWithNonEmptyNeighborhood remainingSeats
    print $ map seatId seatsWithNonEmptyNeighborhood



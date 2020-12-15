
type Action = (Char, Int)
type Actions = [Action]

type Vec2 = (Int, Int)
type Yaw = Int

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (ax, ay) (bx, by) = (ax+bx, ay+by)

type ShipState = (Vec2, Yaw)

addYaw :: Yaw -> Yaw -> Yaw 
addYaw a b = (a + b) `mod` 360

getDirectionFromYaw :: Yaw -> Char
getDirectionFromYaw 0 = 'E'
getDirectionFromYaw 90 = 'N'
getDirectionFromYaw 180 = 'W'
getDirectionFromYaw 270 = 'S'

applyAction :: ShipState -> Action -> ShipState
applyAction (vec2, yaw) ('N', value) = (vec2 `addVec2` (0, value), yaw)
applyAction (vec2, yaw) ('S', value) = (vec2 `addVec2` (0, -value), yaw)
applyAction (vec2, yaw) ('E', value) = (vec2 `addVec2` (value, 0), yaw)
applyAction (vec2, yaw) ('W', value) = (vec2 `addVec2` (-value, 0), yaw)
applyAction (vec2, yaw) ('L', value) = (vec2, yaw `addYaw` value)
applyAction (vec2, yaw) ('R', value) = (vec2, yaw `addYaw` (-value) )
applyAction (vec2, yaw) ('F', value) = applyAction (vec2, yaw) (getDirectionFromYaw yaw, value)


lineToAction :: String -> Action
lineToAction line =
    ( head line, ((read :: String -> Int) . drop 1) line)

getActionsFromFile :: String -> IO Actions
getActionsFromFile fname = do
    content <- readFile fname
    let actionLines = lines content
    return $ map lineToAction actionLines

start :: ShipState
start = ((0, 0), 0)

applyActions :: Actions -> ShipState -> ShipState
applyActions actions state =
    foldl applyAction state actions

manhDist :: ShipState -> ShipState -> Int
manhDist ((ax, ay), _)  ((bx, by), _) = abs (ax-bx) + abs (ay-by)

main :: IO ()
main = do
    actions <- getActionsFromFile "input_12a.txt"
    let end1 = applyActions actions start
    print $ end1
    print $ manhDist end1 start


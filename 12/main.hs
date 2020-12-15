
type Action = (Char, Int)
type Actions = [Action]

type Vec2 = (Int, Int)
type Yaw = Int

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (ax, ay) (bx, by) = (ax+bx, ay+by)

subVec2 :: Vec2 -> Vec2 -> Vec2
subVec2 (ax, ay) (bx, by) = (ax-bx, ay-by)


rotateVec2 :: Vec2 -> Int -> Vec2
rotateVec2 (ax, ay) degrees =
    let fx = fromIntegral ax
        fy = fromIntegral ay
        rad = fromIntegral degrees / 180.0  * pi :: Float
        bx = fx * cos rad  - fy * sin  rad
        by = fx * sin rad  + fy * cos  rad
    in (round bx, round by)

type WaypointState = (Vec2, Yaw)
type ShipState = (Vec2, WaypointState)

addYaw :: Yaw -> Yaw -> Yaw
addYaw a b = (a + b) `mod` 360

getDirectionFromYaw :: Yaw -> Char
getDirectionFromYaw 0 = 'E'
getDirectionFromYaw 90 = 'N'
getDirectionFromYaw 180 = 'W'
getDirectionFromYaw 270 = 'S'

applyAction :: WaypointState -> Action -> WaypointState
applyAction (vec2, yaw) ('N', value) = (vec2 `addVec2` (0, value), yaw)
applyAction (vec2, yaw) ('S', value) = (vec2 `addVec2` (0, -value), yaw)
applyAction (vec2, yaw) ('E', value) = (vec2 `addVec2` (value, 0), yaw)
applyAction (vec2, yaw) ('W', value) = (vec2 `addVec2` (-value, 0), yaw)
applyAction (vec2, yaw) ('L', value) = (vec2, yaw `addYaw` value)
applyAction (vec2, yaw) ('R', value) = (vec2, yaw `addYaw` (-value) )
applyAction (vec2, yaw) ('F', value) = applyAction (vec2, yaw) (getDirectionFromYaw yaw, value)


rotateWaypoint :: WaypointState -> Int -> WaypointState
rotateWaypoint (vec2, yaw) degrees =
    (rotateVec2 vec2 degrees, yaw)

imulVec2 :: Vec2 -> Int -> Vec2
imulVec2 (ax, bx) m = (ax * m, bx * m)

applyAction2 :: ShipState -> Action -> ShipState
applyAction2 (vec2, waypointState) ('L', value) = (vec2, rotateWaypoint waypointState value    )
applyAction2 (vec2, waypointState) ('R', value) = (vec2, rotateWaypoint waypointState (-value) )
applyAction2 (vec2, waypointState@(waypointVec2, _)) ('F', value) = (vec2 `addVec2` (imulVec2 waypointVec2 value), waypointState) 
applyAction2 (shipVec2, waypointState) action = (shipVec2, applyAction waypointState action)


lineToAction :: String -> Action
lineToAction line =
    ( head line, ((read :: String -> Int) . drop 1) line)

getActionsFromFile :: String -> IO Actions
getActionsFromFile fname = do
    content <- readFile fname
    let actionLines = lines content
    return $ map lineToAction actionLines

start :: WaypointState
start = ((0, 0), 0)

startShipState :: ShipState
startShipState = ((0, 0), ((10, 1), 0))

applyActions :: Actions -> WaypointState -> WaypointState
applyActions actions state =
    foldl applyAction state actions

applyActions2 :: Actions -> ShipState -> ShipState
applyActions2 actions state =
    foldl applyAction2 state actions

manhDist :: Vec2 -> Vec2 -> Int
manhDist (ax, ay) (bx, by) = abs (ax-bx) + abs (ay-by)


main :: IO ()
main = do
    actions <- getActionsFromFile "input_12a.txt"
    let end1 = applyActions actions start
    print $ end1
    print $ manhDist (fst end1) (fst start)
    let end2 = applyActions2 actions startShipState
    print $ end2
    print $ manhDist (fst end2) (fst startShipState)


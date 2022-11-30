module Breakout (
    Breakout,
    gameLoop,
    processGame,
    updateBat,
    updateBall,
    updateBricks
) where
    
import Object 
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime, NominalDiffTime)
import Data.Time (NominalDiffTime)
import Object (Ball(bposition, bvelocity), Bat (batposition, batvelocity))
import Geometry ( Vector2D, (^+^), (^*^), Vector2 (Vector2) )

data Breakout = Breakout {
    mode :: GameMode,

    startTime :: UTCTime,
    lastStepTime :: UTCTime,

    bat :: Bat,
    ball :: Ball,
    bricks :: [Brick],
    board :: Board
}

data GameMode = Play | Start | Over
    deriving (Eq, Show)

gameLoop :: Breakout -> IO ()
gameLoop Breakout {mode = Start} = putStrLn "Push to Start"
gameLoop g = do
    curTime <- getCurrentTime
    let timeDiff = realToFrac (diffUTCTime curTime (lastStepTime g))

    -- process keyboard event and change the speed of the objects
    -- TODO let game' = processIO g events
    let game' = g

    let game'' = processGame timeDiff game' -- update objects

    if mode game'' == Play then 
        do
            -- render
            gameLoop game'
    else putStrLn "test"

processGame :: Double -> Breakout -> Breakout 
processGame t g = 
    -- update lastStepTime, bat, bricks
    g {bat = updateBat t (bat g), ball = updateBall t (ball g) g, 
            bricks = updateBricks t (bricks g)}


updateBat :: Double -> Bat -> Bat 
updateBat t b = b {batposition = p + round (t * v) }
    where p = batposition b
          v = batvelocity b

-- update Ball position and velocity
updateBall :: Double -> Ball -> Breakout -> Ball 
updateBall t b g =
    case collision of 
        Collision p t' bricks -> getNewBall t' b
        None -> newBall
    where newBall = getNewBall t b 
          collision = checkCollision newBall g

checkCollision :: Ball -> Breakout -> Collision
checkCollision ball g = do
        curTime <- getCurrentTime

    where
        checkBricks ball _ [] = None
        checkBricks ball prev [x:xs] = 
            case checkWithinBrick x of
                None -> checkBricks ball (prev++x) xs
                _ -> Collision (bposition ball) curTime prev++xs 


-- assume brick position is the upper left corner of the brick
checkWithinBrick:: Ball -> Brick -> Brick
checkWithinBrick ball brick = do
    let Vector2 ballx bally = bposition ball
    let Vector2 brickx bricky = briposition brick
    let bw = briWidth brick
    let bh = briHeight brick 
    if ballx >= brickx && ballx <= brickx + bw && bally >= bricky && bally <= bricky + bh
        then brick
    else
        None

checkBorder::Ball -> [Brick] -> Board -> UTCTime-> Collision
checkBorder ball bricks board t= do
    let boardW = boardWidth board 
    let boardH = boardHeight board
    let Vector2 ballx bally = bposition ball
    if ballx <=0 || bally<= 0 || bally >= boardH || ballx >= boardW
        then Collision (bposition ball) t bricks
    else None

-- assume bat position is left corner
checkCollBat:: Ball ->[Brick] -> Board->UTCTime -> Collision
checkCollBat ball brick board t = do
    let Vector2 ballx bally = bposition ball
    if bally >= bheight bat && ballx >= batposition bat && ballx <= (batposition bat) + (bwith bat)
        then Collision (bposition ball) t brick
    else None
    

getNewBall :: Double -> Ball -> Ball
getNewBall t b = b {bposition = orig ^+^ (t ^*^ v)}
                where orig = bposition b
                      v = bvelocity b
                      
data Collision = Collision {position :: Vector2D, 
                            time :: Double, cbricks :: [Brick]} | None
    deriving (Eq, Show)


updateBricks :: Double -> [Brick] -> [Brick]
updateBricks t b =
    b -- todo


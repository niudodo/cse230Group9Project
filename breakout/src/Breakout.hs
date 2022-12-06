module Breakout (
    Breakout,
    gameLoop,
    processGame,
    updateBat,
    updateBall,
    updateBricks
) where
    
import Object
    ( Ball(..),
      Bat(batvelocity, batposition, bwidth, bheight),
      Board(boardWidth, boardHeight),
      Brick(briposition, briWidth, briHeight)) 
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime, NominalDiffTime)
import Data.Time (NominalDiffTime, UTCTime (utctDayTime))
import Geometry ( Vector2D, (^+^), (^*^), Vector2 (Vector2) )
import GHC.Float (int2Double)
import qualified Brick.Types as T
import Brick.Types (locationRowL, locationColumnL, Location(..), Widget)

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

ballRadius :: Int
ballRadius = 5

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
          collision = checkCollision t newBall g


checkCollision :: Double -> Ball -> Breakout ->  Collision
checkCollision curTime ball g = do
    let collBat = checkCollBat ball (bat g) (bricks g) curTime
    let collBoard = checkBorder ball (bricks g) (board g) curTime
    let collBrick = checkBricks ball [] (bricks g) curTime
    if collBat /= None
        then collBat
    else if collBoard /= None 
        then collBoard
    else  
        collBrick
    where
        checkBricks:: Ball -> [Brick] -> [Brick]-> Double-> Collision
        checkBricks ball _ [] curTime = None
        checkBricks ball prev (x:xs) curTime
            | checkWithinBrick ball x /=0 = Collision (reflectBall ball (checkWithinBrick ball x) 0.0) curTime (prev++xs) 
            | otherwise = checkBricks ball (prev++[x]) xs curTime


-- assume brick position is the upper left corner of the brick
checkWithinBrick:: Ball -> Brick -> Int
checkWithinBrick ball brick = do
    let Location (ballx, bally) = bposition ball
    let Location (brickx, bricky) =  briposition brick
    let bw = briWidth brick
    let bh = briHeight brick 
    -- if ball hit on leftside/right of brick
    if (ballx + ballRadius >= brickx && ballx <= bricky && bally >= bricky && bally <= bricky + bh)
        ||( ballx - ballRadius <= brickx + bw && ballx >= brickx + bw && bally >= bricky && bally <= bricky + bh)
        then 2
    -- if ball hit on top/bottom of brick
    else if (bally + ballRadius >= bricky && bally <= bricky && ballx >= brickx && ballx <= brickx + bw)
        ||( bally - ballRadius <= bricky + bh && bally >= bricky + bh && ballx >= brickx && ballx <= brickx + bh)
        then 1
    else 0

checkBorder::Ball -> [Brick] -> Board -> Double-> Collision
checkBorder ball bricks board t= do
    let boardW = boardWidth board
    let boardH = boardHeight board
    let Location (ballx, bally) = bposition ball
    if ballx <=0 || ballx >= boardW
        then Collision (reflectBall ball 2 0.0) t bricks
    else if bally <=0 || bally >= boardH
        then Collision (reflectBall ball 1 0.0 ) t bricks
    else None


-- assume bat position is left corner
checkCollBat:: Ball -> Bat ->[Brick] ->Double -> Collision
checkCollBat ball bat brick t = do
    let Location (ballx, bally) = bposition ball
    if bally + ballRadius >=  bheight bat && ballx >= batposition bat && ballx <= batposition bat + bwidth bat
        then Collision (reflectBall ball 1 (batvelocity bat)) t brick
    else None


-- 1 for vertical 
-- 2 for horizontial 
reflectBall:: Ball -> Int -> Double -> Ball
reflectBall ball dir offset = Ball (bposition ball) velocity
    where
        Vector2 xv yv = bvelocity ball
        velocity = case dir of 
            1 -> Vector2 (xv + offset) (-1.0 * yv)  
            _ -> Vector2 (-1.0 * xv + offset) yv 
    

getNewBall :: Double -> Ball -> Ball
getNewBall t b = b {bposition = orig ^+^ (t ^*^ v)}
                where orig = bposition b
                      v = bvelocity b
                      
data Collision = Collision {cball :: Ball, 
                            time :: Double, cbricks :: [Brick]} | None
    deriving (Eq, Show)


updateBricks :: Double -> [Brick] -> [Brick]
updateBricks t b =
    b -- todo


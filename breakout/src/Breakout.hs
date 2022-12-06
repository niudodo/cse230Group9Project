module Breakout (
    Breakout,
    gameLoop,
    processGame,
    updateBat,
    updateBall,
    updateBricks
) where
    
import Object
    ( Ball(bvelocity, bposition),
      Bat(batvelocity, batposition),
      Board(boardWidth, boardHeight),
      Brick ) 
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime, NominalDiffTime)
import Data.Time (NominalDiffTime)
import Object (Ball(bposition, bvelocity), Bat (batposition, batvelocity))
import Geometry ( Vector2D, (^+^), (^*^) )

data Breakout = Breakout {
    mode :: GameMode,
    score :: Int,

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

initGame :: n -> Breakout 
initGame n = Breakout {
    mode = Play,
    score = 0,
    bat = Bat {
        batposition = 50,
        bwidth = 20,
        batvelocity = 2
    },
    bricks = genBrick n,
    board = Board {boardHeight = 200, boardWitdth = 100}
}

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
checkCollision ball g = None -- todo

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


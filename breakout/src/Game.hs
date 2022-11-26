module Game where
    
import Object
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime, NominalDiffTime)
import Data.Time (NominalDiffTime)

data Game = Game {
    mode :: GameMode,

    startTime :: UTCTime,
    lastStepTime :: UTCTime,

    bat :: Bat,
    ball :: Ball,
    bricks :: [Brick]
}

data GameMode = Play | Start | Over
    deriving (Eq, Show)

gameLoop :: Game -> IO ()
gameLoop Game {mode = Start} = putStrLn "Push to Start"
gameLoop g = do
    curTime <- getCurrentTime
    let timeDiff = diffUTCTime curTime (lastStepTime g)

    let game' = processGame timeDiff g

    if mode game' == Play then 
        gameLoop game'
    else putStrLn "test"
    
processGame :: NominalDiffTime -> Game -> Game 
processGame t g = 
    -- update lastStepTime, bat, bricks
         g -- {bat = updateBat t bat, ball = updateBall t ball, bricks = updateBricks t bricks}

updateBat :: NominalDiffTime -> Bat -> Bat 
updateBat t b =
    b -- todo b + v * t

updateBall :: NominalDiffTime -> Ball -> Ball 
updateBall t b =
    b 

updateBricks :: NominalDiffTime -> [Brick] -> [Brick]
updateBricks t b =
    b
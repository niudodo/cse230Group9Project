module Game where
    
import Object
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime)

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
gameLoop game@Game{mode, startTime, lastStepTime, bat, ball, bricks} = do 
    let curTime = getCurrentTime
    let timeDiff = curTime - lastStepTime

    let game' = processGame timeDiff game

    if mode game' == Play then 
        gameLoop game'
    else return
    
processGame :: NorminalDiffTime -> Game -> Game 
processGame t g@Game{mode, startTime, lastStepTime, bat, ball, bricks} = 
    -- update lastStepTime, bat, bricks
    Game {mode, startTime, (addUTCTime lastStepTime t),
         updateBat t bat, updateBall t ball, updateBrick}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Breakout (
    Breakout(..),
    -- gameLoop,
    -- processGame,
    updateBat,
    updateBall,
    updateBricks,
    initGame,
    shiftBat,
    timeStep,
    GameMode
) where
    
import Object
    ( Ball(..),
      Bat(..),
      Board(..),
      Brick(..) )

import Control.Lens hiding (Empty) 

import Control.Monad (forM_, mfilter, when, (<=<))
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.State (StateT(..), get, evalStateT, execStateT)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime, NominalDiffTime)
import Data.Time (NominalDiffTime, UTCTime (utctDayTime))
import Geometry ( Vector2D, (^+^), (^*^), Vector2 (Vector2) )
import GHC.Float (int2Double)

data GameMode = Play | Start | Over
    deriving (Eq, Show)
data Breakout = Breakout {
    _mode :: GameMode,
    _score :: Int,

    _bat :: Bat,
    _ball :: Ball,
    _bricks :: [Brick],
    _board :: Board
} deriving (Eq, Show)
makeLenses ''Breakout

type BreakoutT = StateT Breakout
type BreakoutM a = forall m. (Monad m) => BreakoutT m a

evalBreakoutM :: BreakoutM a -> Breakout -> a
evalBreakoutM m = runIdentity . evalStateT m

execBreakoutM :: BreakoutM a -> Breakout -> Breakout
execBreakoutM m = runIdentity . execStateT m

timeStep :: MonadIO m => BreakoutT m ()
timeStep = do
    g <- get
    b <- use ball
    bt <- use bat
    brks <- use bricks
    brd <- use board
    let t = 1.0 
    bat .= updateBat t bt
    ball .= updateBall t b g 
    bricks .= updateBricks t brks




-- processGame :: Double -> Breakout -> Breakout 
-- processGame t g = 
--     -- update lastStepTime, bat, bricks
--     g {bat = updateBat t (_bat g), ball = updateBall t (ball g) g, 
--             bricks = updateBricks t (bricks g)}

ballRadius :: Double
ballRadius = 5.0

-- gameLoop :: Breakout -> IO ()
-- gameLoop Breakout {mode = Start} = putStrLn "Push to Start"
-- gameLoop g = do
--     curTime <- getCurrentTime
--     let timeDiff = realToFrac (diffUTCTime curTime (lastStepTime g))

--     -- process keyboard event and change the speed of the objects
--     -- TODO let game' = processIO g events
--     let game' = g

--     let game'' = processGame timeDiff game' -- update objects

--     if mode game'' == Play then 
--         do
--             -- render
--             gameLoop game'
--     else putStrLn "test"



initGame :: Int -> Breakout 
initGame n = Breakout {
    _mode = Play,
    _score = 0,
    _bat = Bat {
        batposition = 50,
        bwidth = 20,
        batvelocity = 2
    },
    _bricks = genBricks n 3, -- TODO
    _board = Board {boardHeight = 200, boardWidth = 100}
}

shiftBat :: Int -> Breakout -> Breakout
shiftBat n g = g {_bat = getShiftBat n $ _bat g}

getShiftBat :: Int -> Bat -> Bat
getShiftBat 0 b = b {batvelocity = -2}
getShiftBat 1 b = b {batvelocity = 2}




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
    let collBat = checkCollBat ball (_bat g) (_bricks g) curTime
    let collBoard = checkBorder ball (_bricks g) (_board g) curTime
    let collBrick = checkBricks ball [] (_bricks g) curTime
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
    let Vector2 ballx bally = bposition ball
    let Vector2 brickx bricky = briposition brick
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
    let boardW = int2Double (boardWidth board)
    let boardH = int2Double (boardHeight board)
    let Vector2 ballx bally = bposition ball
    if ballx <=0 || ballx >= boardW
        then Collision (reflectBall ball 2 0.0) t bricks
    else if bally <=0 || bally >= boardH
        then Collision (reflectBall ball 1 0.0 ) t bricks
    else None


-- assume bat position is left corner
checkCollBat:: Ball -> Bat ->[Brick] ->Double -> Collision
checkCollBat ball bat brick t = do
    let Vector2 ballx bally = bposition ball
    if bally + ballRadius >= int2Double (bheight bat) && ballx >= int2Double (batposition bat) && ballx <= int2Double (batposition bat + bwidth bat)
        then Collision (reflectBall ball 1 (batvelocity bat)) t brick
    else None


-- 1 for vertical 
-- 2 for horizontial 
reflectBall:: Ball -> Int -> Double -> Ball
reflectBall ball dir offset = Ball pos velocity
    where
        pos = bposition ball
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

genBricks:: Int-> Double ->[Brick]
genBricks 0 _ = []
genBricks n posY = genBricksRow 10 0 posY ++ genBricks (n-1) (posY+5)


genBricksRow:: Int-> Double -> Double ->[Brick]
genBricksRow 0 _ _ = []  
genBricksRow n posX posY = new_brick ++ genBricksRow (n-1) new_posX posY
    where 
        new_posX = posX + 5
        new_brick = [Brick{
            briposition = Vector2 new_posX posY,
            briWidth = 5,
            briHeight = 5
        }] 

module Object where 

import Geometry (Vector2D)

data Ball = Ball {
    bposition :: Vector2D,
    bvelocity :: Vector2D
} deriving (Eq, Show)

data Bat = Bat {
    batposition :: Int,
    bwidth :: Int,
    bheight :: Int,
    batvelocity :: Double
} deriving (Eq, Show)

data Brick = Brick {
    briposition :: Vector2D,
    briWidth :: Double,
    briHeight :: Double
} deriving (Eq, Show)

data Board = Board {
    boardWidth :: Int,
    boardHeight :: Int
} deriving (Eq, Show)

genBrick :: n -> [Brick]
genBrick 0 = [Brick {
        briposition = Vector2D 20.0 100.0, 
        briWidth = 10.0, 
        briHeight = 10.0
        }]
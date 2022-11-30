module Object where 

import Geometry

data Ball = Ball {
    bposition :: Vector2D,
    bvelocity :: Vector2D
} deriving (Eq, Show)

data Bat = Bat {
    batposition :: Int,
    bwidth :: Int,
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
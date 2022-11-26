module Object where 

import Geometry

data Ball = Ball {
    bposition :: Vector,
    bvelocity :: Vector
}

data Bat = Bat {
    batposition :: Vector,
    bwidth :: Int,
    batv :: Double
}

data Brick = Brick {
    briposition :: Vector,
    briWidth :: Double,
    briHeight :: Double
}
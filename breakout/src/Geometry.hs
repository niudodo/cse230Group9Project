module Geometry where
    
data Point = Point{
    px :: Double,
    py :: Double
} deriving(Eq, Show)

data Vector = Vector {
    vx :: Double,
    vy :: Double
} deriving(Eq, Show)
module Object where 

import Geometry
import qualified Brick.Types as T
import Brick.Types (locationRowL, locationColumnL, Location(..), Widget)

data Ball = Ball {
    bposition :: T.Location,
    bvelocity :: Vector2D
} deriving (Eq, Show)

data Bat = Bat {
    batposition :: Int,
    bwidth :: Int,
    bheight :: Int,
    batvelocity :: Double
} deriving (Eq, Show)

data Brick = Brick {
    briposition :: T.Location,
    briWidth :: Int,
    briHeight :: Int
} deriving (Eq, Show)

data Board = Board {
    boardWidth :: Int,
    boardHeight :: Int
} deriving (Eq, Show)
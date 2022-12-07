module Geometry where

data Vector2 s = Vector2 !s !s deriving (Eq, Ord, Show)
type Vector2D = Vector2 Double   

(^+^),(^-^) :: Num s =>  Vector2 s -> Vector2 s -> Vector2 s
Vector2 x1 y1 ^+^ Vector2 x2 y2 = Vector2 (x1 + x2) (y1 + y2)
Vector2 x1 y1 ^-^ Vector2 x2 y2 = Vector2 (x1 - x2) (y1 - y2)

(^*^) :: Num s => s -> Vector2 s -> Vector2 s
t ^*^ (Vector2 x1 x2) = Vector2 (t * x1) (t * x2)

norm :: (Floating s, Eq s) => Vector2 s -> Vector2 s
norm (Vector2 0.0 0.0) = (Vector2 0.0 0.0)
norm (Vector2 x1 x2) = Vector2 (x1 / mod) (x2 / mod)
                where mod = sqrt (x1 * x1 + x2 * x2)

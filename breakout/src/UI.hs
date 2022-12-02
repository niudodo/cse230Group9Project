module UI (render)
where

import Breakout
import Brick (Widget, simpleMain, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

render :: Breakout -> Widget ()
render breakout =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Hello!") $
    (center (str "Left") <+> center (str "Right"))
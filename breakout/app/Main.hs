module Main (Main.main) where

import UI (app)
import Brick (simpleMain)
import Breakout (Breakout)

main :: IO ()
main = do
    game <- playGame
    

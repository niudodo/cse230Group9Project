module Main (Main.main) where

import UI (app, playGame)
import Brick (simpleMain)
import Breakout (Breakout)

main :: IO ()
main = do
    game <- playGame
    putStrLn "Game Over" 
    


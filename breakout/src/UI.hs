{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module UI where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.Types (locationRowL, locationColumnL, Location(..), Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( translateBy
  , str
  , relativeTo
  , reportExtent
  , withDefAttr
  )
import Brick.Util (fg)
import Brick.AttrMap
  ( attrMap
  , AttrName
  , attrName
  )

import Breakout
import System.Posix.Internals (o_NOCTTY)
import qualified Distribution.Simple.Setup as V
import Language.Haskell.TH (VarBangType)
import Control.Monad.RWS (Any(getAny))

data UI = UI {
  _game :: Breakout, -- game
  _paused :: Bool
}
makeLenses ''UI

data Tick = Tick

Type Name = ()

playGame :: IO Game
playGame = do
  initialGame <- initGame 3
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  ui <- customMain initialVty builder (Just chan) app $ UI {
    _game = initialGame
  }

handleEvent :: BrickEvent Name Tick -> EventM Name UI ()
handleEvent (AppEvent Tick             ) = handleTick
handleEvent (VtyEvent (V.EvKey V.KRight  [])) = handleShift 0
handleEvent (VtyEvent (V.EvKey V.KLeft  [])) = handleShift 1
handleEvent (VtyEvent (V.EvKey V.Kesc [])) = halt
handleEvent _ = pure ()

handleTick :: EventM Name UI ()
handleTick = do 
  ui <- get
  unless (ui ^. game . to mode != Over) $ do
    game .= processGame 1.0 $ ui ^. game 

handleShift :: Int -> EventM Name UI ()
handleLeft n = do 
  ui <- get
  do game .= shiftBat n $ ui ^. game


drawUI :: UI -> [Widget Name]
drawUI ui = 
  -- [drawBall ui ^. game . to ball, drawBat ...]
   B.border $ str "       Bat\n(<- / -> keys move)"

drawBall :: Ball -> Widget Name
drawBall ball = translateBy () $ getLoc ball $ str "O"

drawBricks :: [Brick] -> [Widget Name]

drawBat :: Bat -> Widget Name


arrowAttr :: AttrName
arrowAttr = attrName "attr"

app :: M.App UI Tick Name
app =
    M.App { M.appDraw = drawUI
          , M.appStartEvent = return ()
          , M.appHandleEvent = handleEvent
          , M.appAttrMap = const $ attrMap V.defAttr [(arrowAttr, fg V.cyan)]
          , M.appChooseCursor = M.neverShowCursor
          }
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
  initialGame <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  ui <- customMain initialVty builder Nothing app $ UI {
    _game = initialGame
  }

handleEvent :: BrickEvent Name Tick -> EventM Name UI ()
handleEvent (AppEvent Tick             ) = handleTick
handleEvent (VtyEvent (V.EvKey V.KRight  [])) = exec (shift Right)
handleEvent (VtyEvent (V.EvKey V.KLeft  [])) = exec (shift Left)
handleEvent (VtyEvent (V.EvKey V.Kesc [])) = halt
handleEvent _ = pure ()

handleTick :: EventM Name UI ()
handleTick = do 
  ui <- get
  unless (ui ^. game . to mode != Over) $ do
    game .= processGame 1.0 $ ui ^. game 


drawUI :: UI -> [Widget Name]
drawUI ui = 
  [drawBall ui ^. game . to ball, drawBat ...]

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

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (20, 10))
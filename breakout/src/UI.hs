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

data St =
    St {
         _bottomLayerLocation :: T.Location
       }

makeLenses ''St

data Name =
    MiddleLayerElement
    deriving (Ord, Eq, Show)

drawUi :: St -> [Widget Name]
drawUi st =
    [bottomLayer st]


bottomLayer :: St -> Widget Name
bottomLayer st =
    translateBy (st^.bottomLayerLocation) $
    B.border $ str "       Bat\n(<- / -> keys move)"

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()

-- appEvent (T.VtyEvent (V.EvKey V.KDown  [])) =
--     bottomLayerLocation.locationRowL %= (+ 1)
-- appEvent (T.VtyEvent (V.EvKey V.KUp    [])) =
--     bottomLayerLocation.locationRowL %= (subtract 1)
appEvent (T.VtyEvent (V.EvKey V.KRight [])) =
    bottomLayerLocation.locationColumnL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KLeft  [])) =
    bottomLayerLocation.locationColumnL %= (subtract 1)

appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = return ()

arrowAttr :: AttrName
arrowAttr = attrName "attr"

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr [(arrowAttr, fg V.cyan)]
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (20, 10))
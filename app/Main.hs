{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  )

data Name = VP1
          | VP2
          | VP3
          | VP4
          | VP5
          | VP6
          deriving (Ord, Show, Eq)

drawUi :: () -> [Widget Name]
drawUi = const [ui]
    where
        ui = C.center $ B.border $ hLimit 60 $ vLimit 21 $
             vBox [ imageslist, B.hBorder, containerslist ]

        imageslist = vBox[ str "images", B.hBorder, hBox [ viewport VP1 Vertical $
                      vBox $ str "Names" :
                              str " ":
                             (str <$> ["Name1", "Name2", "Name3"])
                    , B.vBorder
                    , viewport VP2 Vertical $
                      vBox $ str "IDS" :
                              str " ":
                             (str <$> ["ID1", "ID2", "ID3"])
                    , B.vBorder
                    , viewport VP3 Vertical $
                      vBox $ str "ports" :
                              str " ":
                             (str <$> ["port1", "port2", "port3"])
                    ]]

        containerslist = vBox[ str "containers", B.hBorder, hBox [ viewport VP4 Vertical $
                      vBox $ str "Names" :
                              str " ":
                             (str <$> ["Name1", "Name2", "Name3"])
                    , B.vBorder
                    , viewport VP5 Vertical $
                      vBox $ str "IDS" :
                              str " ":
                             (str <$> ["ID1", "ID2", "ID3"])
                    , B.vBorder
                    , viewport VP6 Vertical $
                      vBox $ str "ports" :
                              str " ":
                             (str <$> ["port1", "port2", "port3"])
                    ]]
        

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

vp2Scroll :: M.ViewportScroll Name
vp2Scroll = M.viewportScroll VP2

vp3Scroll :: M.ViewportScroll Name
vp3Scroll = M.viewportScroll VP3

vp4Scroll :: M.ViewportScroll Name
vp4Scroll = M.viewportScroll VP4

vp5Scroll :: M.ViewportScroll Name
vp5Scroll = M.viewportScroll VP5

vp6Scroll :: M.ViewportScroll Name
vp6Scroll = M.viewportScroll VP6

appEvent :: () -> T.BrickEvent Name e -> T.EventM Name (T.Next ())
appEvent _ (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) = M.vScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KUp    [V.MCtrl])) = M.vScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = M.hScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) = M.hScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KRight [])) = M.hScrollBy vp2Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KLeft []))  = M.hScrollBy vp2Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt ()
appEvent _ _ = M.continue ()

app :: M.App () e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app ()

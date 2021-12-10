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

-- Container ls Command
-- ("ID", "Names", "Image", "State", "Status", "Ports")
-- volume ls Command
-- (Name, Mount_Point, Driver, Size)
-- network ls Command
-- (ID, Name, Driver, Scope, Created)

pContainersCmd :: IO ([String], [String], [String], [String], [String], [String])
pContainersCmd = do
    cmds <- genContainersCmd
    case cmds of
      Left _ -> return ([],[],[],[],[],[])
      Right tuples -> return $ tupToList tuples

genContainersCmd :: IO (Either String [(String, String, String, String, String, String)])
genContainersCmd = return $ Right [("aa", "bb", "cc", "dd", "ee", "ff"), ("aaa", "bbb", "ccc", "ddd", "eee", "fff")]

tupToList :: [(String, String, String, String, String, String)] -> ([String], [String], [String], [String], [String], [String])
tupToList [] = ([],[],[],[],[],[])
tupToList (l:ls) = ((l1:ls1), (l2:ls2), (l3:ls3), (l4:ls4), (l5:ls5), (l6:ls6))
    where (l1, l2, l3, l4, l5, l6) = l
          (ls1, ls2, ls3, ls4, ls5, ls6) = tupToList ls

drawUi :: () -> [Widget Name]
drawUi = const [ui]
    where
        ui = C.center $ B.border $ hLimit 60 $ vLimit 21 $
             vBox [str "up and down to scroll top left list", str "bcedf to scroll down other lists", str "BCDEF to scroll up other lists", imageslist, B.hBorder, containerslist ]
        (l1, l2, l3, _, _, _) = pContainersCmd

        imageslist = vBox[ str "images", B.hBorder, hBox [ viewport VP1 Vertical $
                      vBox $ str "Names" :
                              str " ":
                             (str <$> l1)
                    , B.vBorder
                    , viewport VP2 Vertical $
                      vBox $ str "IDS" :
                              str " ":
                             (str <$> l2)
                    , B.vBorder
                    , viewport VP3 Vertical $
                      vBox $ str "ports" :
                              str " ":
                             (str <$> l3)
                    ]]

        containerslist = vBox[ str "containers", B.hBorder, hBox [ viewport VP4 Vertical $
                      vBox $ str "Names" :
                              str " ":
                             (str <$> ["Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9"])
                    , B.vBorder
                    , viewport VP5 Vertical $
                      vBox $ str "IDS" :
                              str " ":
                             (str <$> ["ID1", "ID2", "ID3", "ID4", "ID5", "ID6", "ID7", "ID8", "ID9"])
                    , B.vBorder
                    , viewport VP6 Vertical $
                      vBox $ str "ports" :
                              str " ":
                             (str <$> ["port1", "port2", "port3", "port4", "port5", "port6", "port7", "port8", "port9"])
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
appEvent _ (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = M.hScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) = M.hScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'b') [])) = M.vScrollBy vp2Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'B') []))  = M.vScrollBy vp2Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'c') [])) = M.vScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'C') [])) = M.vScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'd') []))  = M.vScrollBy vp4Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'D') []))    = M.vScrollBy vp4Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'e') [])) = M.vScrollBy vp5Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'E') []))  = M.vScrollBy vp5Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'f') [])) = M.vScrollBy vp6Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'F') []))  = M.vScrollBy vp6Scroll (-1) >> M.continue ()

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

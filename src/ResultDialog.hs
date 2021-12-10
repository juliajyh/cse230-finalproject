module ResultDialog(resultDialog, testResultDialog) where

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T
import Control.Monad (void)


getUI :: String -> D.Dialog Int -> [Widget ()]
getUI body d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str body

appEvent :: D.Dialog Int -> BrickEvent () e -> T.EventM () (T.Next (D.Dialog Int))
appEvent d (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt d
        V.EvKey V.KEnter [] -> M.halt d
        _ -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

getInitialState :: String -> D.Dialog Int
getInitialState title = D.dialog (Just title) (Just (0, choices)) 150
    where
        choices = [ ("OK", 0)]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

getApp :: String -> M.App (D.Dialog Int) e ()
getApp body =
    M.App { M.appDraw = getUI body
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

resultDialog :: String -> String -> IO ()
resultDialog title body = void $ M.defaultMain (getApp body) (getInitialState title)

testResultDialog :: IO ()
testResultDialog = resultDialog "Pull Image" "Successfully pulled image \"fedora\""
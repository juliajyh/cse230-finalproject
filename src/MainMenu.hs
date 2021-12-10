{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module MainMenu where

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

data Name = IVP1
          | IVP2
          | IVP3
          | IVP4
          | IVP5
          | CVP1
          | CVP2
          | CVP3
          | CVP4
          | CVP5
          | CVP6
          | VVP1
          | VVP2
          | VVP3
          | VVP4
          | NVP1
          | NVP2
          | NVP3
          | NVP4
          | NVP5
          deriving (Ord, Show, Eq)


-- Container ls Command
-- ("ID", "Names", "Image", "State", "Status", "Ports")
-- volume ls Command
-- (Name, Mount_Point, Driver, Size)
-- network ls Command
-- (ID, Name, Driver, Scope, Created)
type Image = ([String], [String], [String], [String], [String])
type Container = ([String], [String], [String], [String], [String], [String])
type Volume = ([String], [String], [String], [String])
type Network = ([String], [String], [String], [String], [String])


getUI :: Image -> Container -> Volume -> Network -> () -> [Widget Name]
getUI is cs vs ns = const [ui]
    where
        ui = C.center $ B.border $ hLimit 90 $ vLimit 40 $
             vBox [str "abcde to scroll down, ABCDE to scroll up", B.hBorder, imageslist, B.hBorder, str "fghijk to scroll down, FGHIJK to scroll up", B.hBorder, containerslist, B.hBorder, str "lmno to scroll down, LMNO to scroll up", B.hBorder, volumeslist, B.hBorder, str "pqrst to scroll down, PQRST to scroll up", B.hBorder, networklist]
        (il1, il2, il3, il4, il5) = is
        (cl1, cl2, cl3, cl4, cl5, cl6) = cs
        (vl1, vl2, vl3, vl4) = vs
        (nl1, nl2, nl3, nl4, nl5) = ns

        imageslist = vBox[ str "images", B.hBorder, hBox [ viewport IVP1 Vertical $
                      vBox $ str "ID" :
                              str " ":
                             (str <$> il1)
                    , B.vBorder
                    , viewport IVP2 Vertical $
                      vBox $ str "Name" :
                              str " ":
                             (str <$> il2)
                    , B.vBorder
                    , viewport IVP3 Vertical $
                      vBox $ str "State" :
                              str " ":
                             (str <$> il3)
                    , B.vBorder
                    , viewport IVP4 Vertical $
                      vBox $ str "Status" :
                              str " ":
                             (str <$> il4)
                    , B.vBorder
                    , viewport IVP5 Vertical $
                      vBox $ str "Port" :
                              str " ":
                             (str <$> il5)
                    ]]

        containerslist = vBox[ str "containers", B.hBorder, hBox [ viewport CVP1 Vertical $
                      vBox $ str "ID" :
                              str " ":
                             (str <$> cl1)
                    , B.vBorder
                    , viewport CVP2 Vertical $
                      vBox $ str "Name" :
                              str " ":
                             (str <$> cl2)
                    , B.vBorder
                    , viewport CVP3 Vertical $
                      vBox $ str "Image" :
                              str " ":
                             (str <$> cl3)
                    , B.vBorder
                    , viewport CVP4 Vertical $
                      vBox $ str "State" :
                              str " ":
                             (str <$> cl4)
                    , B.vBorder
                    , viewport CVP5 Vertical $
                      vBox $ str "Status" :
                              str " ":
                             (str <$> cl5)
                    , B.vBorder
                    , viewport CVP6 Vertical $
                      vBox $ str "Port" :
                              str " ":
                             (str <$> cl6)
                    ]]

        -- (Name, Mount_Point, Driver, Size)
        volumeslist = vBox[ str "volumes", B.hBorder, hBox [ viewport VVP1 Vertical $
                      vBox $ str "Name" :
                              str " ":
                             (str <$> vl1)
                    , B.vBorder
                    , viewport VVP2 Vertical $
                      vBox $ str "Mount Point" :
                              str " ":
                             (str <$> vl2)
                    , B.vBorder
                    , viewport VVP3 Vertical $
                      vBox $ str "Driver" :
                              str " ":
                             (str <$> vl3)
                    , B.vBorder
                    , viewport VVP4 Vertical $
                      vBox $ str "Size" :
                              str " ":
                             (str <$> vl4)
                    ]]

        -- (ID, Name, Driver, Scope, Created)
        networklist = vBox[ str "network", B.hBorder, hBox [ viewport NVP1 Vertical $
                      vBox $ str "ID" :
                              str " ":
                             (str <$> nl1)
                    , B.vBorder
                    , viewport NVP2 Vertical $
                      vBox $ str "Name" :
                              str " ":
                             (str <$> nl2)
                    , B.vBorder
                    , viewport NVP3 Vertical $
                      vBox $ str "Driver" :
                              str " ":
                             (str <$> nl3)
                    , B.vBorder
                    , viewport NVP4 Vertical $
                      vBox $ str "Scope" :
                              str " ":
                             (str <$> nl4)
                    , B.vBorder
                    , viewport NVP5 Vertical $
                      vBox $ str "Created" :
                              str " ":
                             (str <$> nl5)
                    ]]

        

ivp1Scroll :: M.ViewportScroll Name
ivp1Scroll = M.viewportScroll IVP1

ivp2Scroll :: M.ViewportScroll Name
ivp2Scroll = M.viewportScroll IVP2

ivp3Scroll :: M.ViewportScroll Name
ivp3Scroll = M.viewportScroll IVP3

ivp4Scroll :: M.ViewportScroll Name
ivp4Scroll = M.viewportScroll IVP4

ivp5Scroll :: M.ViewportScroll Name
ivp5Scroll = M.viewportScroll IVP5

cvp1Scroll :: M.ViewportScroll Name
cvp1Scroll = M.viewportScroll CVP1

cvp2Scroll :: M.ViewportScroll Name
cvp2Scroll = M.viewportScroll CVP2

cvp3Scroll :: M.ViewportScroll Name
cvp3Scroll = M.viewportScroll CVP3

cvp4Scroll :: M.ViewportScroll Name
cvp4Scroll = M.viewportScroll CVP4

cvp5Scroll :: M.ViewportScroll Name
cvp5Scroll = M.viewportScroll CVP5

cvp6Scroll :: M.ViewportScroll Name
cvp6Scroll = M.viewportScroll CVP6

vvp1Scroll :: M.ViewportScroll Name
vvp1Scroll = M.viewportScroll VVP1

vvp2Scroll :: M.ViewportScroll Name
vvp2Scroll = M.viewportScroll VVP2

vvp3Scroll :: M.ViewportScroll Name
vvp3Scroll = M.viewportScroll VVP3

vvp4Scroll :: M.ViewportScroll Name
vvp4Scroll = M.viewportScroll VVP4

nvp1Scroll :: M.ViewportScroll Name
nvp1Scroll = M.viewportScroll NVP1

nvp2Scroll :: M.ViewportScroll Name
nvp2Scroll = M.viewportScroll NVP2

nvp3Scroll :: M.ViewportScroll Name
nvp3Scroll = M.viewportScroll NVP3

nvp4Scroll :: M.ViewportScroll Name
nvp4Scroll = M.viewportScroll NVP4

nvp5Scroll :: M.ViewportScroll Name
nvp5Scroll = M.viewportScroll NVP5


appEvent :: () -> T.BrickEvent Name e -> T.EventM Name (T.Next ())
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'a') [])) = M.vScrollBy ivp1Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'A') []))  = M.vScrollBy ivp1Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'b') [])) = M.vScrollBy ivp2Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'B') []))  = M.vScrollBy ivp2Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'c') [])) = M.vScrollBy ivp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'C') []))  = M.vScrollBy ivp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'd') [])) = M.vScrollBy ivp4Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'D') []))  = M.vScrollBy ivp4Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'e') [])) = M.vScrollBy ivp5Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'E') []))  = M.vScrollBy ivp5Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'f') [])) = M.vScrollBy cvp1Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'F') []))  = M.vScrollBy cvp1Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'g') [])) = M.vScrollBy cvp2Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'G') []))  = M.vScrollBy cvp2Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'h') [])) = M.vScrollBy cvp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'H') []))  = M.vScrollBy cvp3Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'i') [])) = M.vScrollBy cvp4Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'I') []))  = M.vScrollBy cvp4Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'j') [])) = M.vScrollBy cvp5Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'J') []))  = M.vScrollBy cvp5Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'k') [])) = M.vScrollBy cvp6Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'K') []))  = M.vScrollBy cvp6Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'l') [])) = M.vScrollBy vvp1Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'L') []))  = M.vScrollBy vvp1Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'm') [])) = M.vScrollBy vvp2Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'M') []))  = M.vScrollBy vvp2Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'n') [])) = M.vScrollBy vvp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'N') []))  = M.vScrollBy vvp3Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'o') [])) = M.vScrollBy vvp4Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'O') []))  = M.vScrollBy vvp4Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'p') [])) = M.vScrollBy nvp1Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'P') []))  = M.vScrollBy nvp1Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.vScrollBy nvp2Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'Q') []))  = M.vScrollBy nvp2Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'r') [])) = M.vScrollBy nvp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'R') []))  = M.vScrollBy nvp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 's') [])) = M.vScrollBy nvp4Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'S') []))  = M.vScrollBy nvp4Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey (V.KChar 't') [])) = M.vScrollBy nvp5Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey (V.KChar 'T') []))  = M.vScrollBy nvp5Scroll (-1) >> M.continue ()

appEvent _ (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt ()
appEvent _ _ = M.continue ()

app :: Image -> Container -> Volume -> Network -> M.App () e Name
app i c v n =
    M.App { M.appDraw = getUI i c v n
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }

-- handle image
pImagesCmd :: IO Image
pImagesCmd = do
    cmds <- genImagesCmd
    case cmds of
      Left _ -> return ([],[],[],[],[])
      Right tuples -> return $ tupToImagesList tuples

genImagesCmd :: IO (Either String [(String, String, String, String, String)])
genImagesCmd = return $ Right [("aa", "bb", "cc", "dd", "ee"), ("aaa", "bbb", "ccc", "ddd", "eee"), ("aa", "bb", "cc", "dd", "ee"), ("aaa", "bbb", "ccc", "ddd", "eee")]

tupToImagesList :: [(String, String, String, String, String)] -> Image
tupToImagesList [] = ([],[],[],[],[])
tupToImagesList (l:ls) = ((l1:ls1), (l2:ls2), (l3:ls3), (l4:ls4), (l5:ls5))
    where (l1, l2, l3, l4, l5) = l
          (ls1, ls2, ls3, ls4, ls5) = tupToImagesList ls


-- handle container
pContainersCmd :: IO Container
pContainersCmd = do
    cmds <- genContainersCmd
    case cmds of
      Left _ -> return ([],[],[],[],[],[])
      Right tuples -> return $ tupToContainersList tuples


genContainersCmd :: IO (Either String [(String, String, String, String, String, String)])
genContainersCmd = return $ Right [("aa", "bb", "cc", "dd", "ee", "ff"), ("aaa", "bbb", "ccc", "ddd", "eee", "fff"), ("aa", "bb", "cc", "dd", "ee", "ff"), ("aaa", "bbb", "ccc", "ddd", "eee", "fff")]

tupToContainersList :: [(String, String, String, String, String, String)] -> Container
tupToContainersList [] = ([],[],[],[],[],[])
tupToContainersList (l:ls) = ((l1:ls1), (l2:ls2), (l3:ls3), (l4:ls4), (l5:ls5), (l6:ls6))
    where (l1, l2, l3, l4, l5, l6) = l
          (ls1, ls2, ls3, ls4, ls5, ls6) = tupToContainersList ls


-- handle volume
pVolumesCmd :: IO Volume
pVolumesCmd = do
    cmds <- genVolumesCmd
    case cmds of
      Left _ -> return ([],[],[],[])
      Right tuples -> return $ tupToVolumesList tuples

genVolumesCmd :: IO (Either String [(String, String, String, String)])
genVolumesCmd = return $ Right [("aa", "bb", "cc", "dd"), ("aaa", "bbb", "ccc", "ddd"), ("aa", "bb", "cc", "dd"), ("aaa", "bbb", "ccc", "ddd")]

tupToVolumesList :: [(String, String, String, String)] -> Volume
tupToVolumesList [] = ([],[],[],[])
tupToVolumesList (l:ls) = ((l1:ls1), (l2:ls2), (l3:ls3), (l4:ls4))
    where (l1, l2, l3, l4) = l
          (ls1, ls2, ls3, ls4) = tupToVolumesList ls


-- handle network
pNetworkCmd :: IO Network
pNetworkCmd = do
    cmds <- genNetworkCmd
    case cmds of
      Left _ -> return ([],[],[],[],[])
      Right tuples -> return $ tupToNetworkList tuples


genNetworkCmd :: IO (Either String [(String, String, String, String, String)])
genNetworkCmd = return $ Right [("aa", "bb", "cc", "dd", "ee"), ("aaa", "bbb", "ccc", "ddd", "eee"), ("aa", "bb", "cc", "dd", "ee"), ("aaa", "bbb", "ccc", "ddd", "eee")]

tupToNetworkList :: [(String, String, String, String, String)] -> Network
tupToNetworkList [] = ([],[],[],[],[])
tupToNetworkList (l:ls) = ((l1:ls1), (l2:ls2), (l3:ls3), (l4:ls4), (l5:ls5))
    where (l1, l2, l3, l4, l5) = l
          (ls1, ls2, ls3, ls4, ls5) = tupToNetworkList ls



mainMenu :: IO ()
mainMenu = do
  container <- pContainersCmd
  image <- pImagesCmd
  volume <- pVolumesCmd
  network <- pNetworkCmd
  void $ M.defaultMain (app image container volume network) ()


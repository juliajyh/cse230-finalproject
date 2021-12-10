module MainMenu(testMainMenu, mainMenu) where

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import Brick.Types ( Widget, BrickEvent )
import qualified Brick.Types as T
import Brick.Widgets.Core ( str )
import qualified Brick.AttrMap as A
import Backend
    ( Command(..),
      NetworkLsInfo,
      VolumeLsInfo,
      ImageLsInfo,
      ContainerLsInfo,
      psCmd,
      imageLsCmd,
      volumeLsCmd,
      networkLsCmd )

drawUI :: Command -> [Widget ()]
drawUI _ = [ui]
    where
        ui = str $ 
            "Press 'r' to run container; Press 'i' to pull image; Press 'd' to remove container;\n" <>
            "Press 'x' to remove image; Press 'e' to execute command; Press 'a' to start container;\n" <>
            "Press 's' to stop container; Press <Enter> to refresh; Press <Esc> to quit" 

appEvent :: Command -> BrickEvent () e -> T.EventM () (T.Next Command)
appEvent d (T.VtyEvent ev) = 
    case ev of 
        V.EvKey V.KEsc [] -> M.halt DockerHalt
        V.EvKey V.KEnter [] -> M.halt DockerMain
        V.EvKey (V.KChar 'r') [] -> M.halt DockerRun 
        V.EvKey (V.KChar 'i') [] -> M.halt DockerImagePull
        V.EvKey (V.KChar 'd') [] -> M.halt DockerRm 
        V.EvKey (V.KChar 'x') [] -> M.halt DockerImageRm 
        V.EvKey (V.KChar 'e') [] -> M.halt DockerExec
        V.EvKey (V.KChar 'a') [] -> M.halt DockerStart 
        V.EvKey (V.KChar 's') [] -> M.halt DockerStop

        _ -> M.continue d 
appEvent d _ = M.continue d

initialState :: Command 
initialState = DockerMain

app :: M.App Command e ()
app =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const $ A.attrMap V.defAttr []
          }

mainMenu :: ContainerLsInfo -> ImageLsInfo -> VolumeLsInfo -> NetworkLsInfo -> IO Command
mainMenu _ _ _ _ = M.defaultMain app DockerMain

testMainMenu :: IO ()
testMainMenu = do 
    containers <- psCmd
    images <- imageLsCmd
    volumes <- volumeLsCmd
    networks <- networkLsCmd
    c <- mainMenu containers images volumes networks
    putStrLn $ "You chose: " <> show c

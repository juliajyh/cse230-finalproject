module TestBrickStates(testBrickStates) where

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import Brick.Types ( Widget, BrickEvent )
import qualified Brick.Types as T
import Brick.Widgets.Core ( str )
import qualified Brick.AttrMap as A

data Command = 
    DockerMain | DockerImagePull | DockerImageRm | DockerRun | DockerRm | DockerExec | DockerHalt
    deriving (Show)

drawUI :: Command -> [Widget ()]
drawUI _ = [ui]
    where
        ui = str "Press 'r' to run container; Press 'i' to pull image; Press 'd' to remove container;\npress 'x' to remove image; press 'e' to execute command"

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

testBrickStates :: IO ()
testBrickStates = do 
    c <- M.defaultMain app initialState
    putStrLn $ "You chose: " <> show c
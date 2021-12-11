{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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
import Data.Text.Conversions
      
import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (center, hCenter, vCenter)

getUI :: ContainerLsInfo -> ImageLsInfo -> VolumeLsInfo -> NetworkLsInfo -> Command -> [Widget ()]
getUI containers images volumes networks _ = [ui]
    where
        ui = center $ 
            (hCenter $ txt "IMAGES") <=>
            (hCenter $ renderTable $ imageLs2Table images) <=>
            (hCenter $ txt "CONTAINERS") <=> 
            (hCenter $ renderTable $ containerLs2Table containers) <=>
            (hCenter $ txt "VOLUMES") <=>
            (hCenter $ renderTable $ volumeLs2Table volumes) <=>
            (hCenter $ txt "NETWORKS") <=>
            (hCenter $ renderTable $ networkLs2Table networks) <=>
            help 
            where 
            help = hCenter $ padTop (Pad 1) body
            body = str $ 
                        "Press 'r' to run container;    Press 'i' to pull image;        Press 'd' to remove container;\n" <>
                        "Press 'x' to remove image;     Press 'e' to execute command;   Press 'a' to start container;\n" <>
                        "Press 's' to stop container;   Press <Enter> to refresh;       Press <Esc> to quit"
            
            
        -- ui = str $ 
 

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

getApp :: ContainerLsInfo -> ImageLsInfo -> VolumeLsInfo -> NetworkLsInfo -> M.App Command e ()
getApp containers images volumes networks =
    M.App { M.appDraw = getUI containers images volumes networks
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const $ A.attrMap V.defAttr []
          }

container2Widget :: (String, String, String, String, String, String) -> [Widget ()]
container2Widget (id, n, image, state, status, ports) = 
    [txt $ toText id, txt $ toText n, txt $ toText image, txt $ toText state, txt $ toText status, txt $ toText ports]

containerLs2Widgets :: ContainerLsInfo -> [[Widget ()]]
containerLs2Widgets s = [[txt "ID", txt "NAME", txt "IMAGE", txt "STATE", txt "STATUS", txt "PORTS"]] ++ (map container2Widget s)

containerLs2Table :: ContainerLsInfo -> Table ()
containerLs2Table s = setDefaultColAlignment AlignCenter $ rowBorders False $ table $ containerLs2Widgets s

image2Widget :: (String, String, String, String, String) -> [Widget ()]
image2Widget (id, repo, tag, created, size) = 
    [txt $ toText id, txt $ toText repo, txt $ toText tag, txt $ toText created, txt $ toText size]

imageLs2Widgets :: ImageLsInfo -> [[Widget ()]]
imageLs2Widgets s = [[txt "ID", txt "REPO", txt "TAG", txt "CREATED", txt "SIZE"]] ++ (map image2Widget s)

imageLs2Table :: ImageLsInfo -> Table ()
imageLs2Table s = setDefaultColAlignment AlignCenter $ rowBorders False $ table $ imageLs2Widgets s

volume2Widget :: (String, String, String, String) -> [Widget ()]
volume2Widget (name, mount, driver, size) = 
    [txt $ toText name, txt $ toText mount, txt $ toText driver, txt $ toText size]

volumeLs2Widgets :: VolumeLsInfo -> [[Widget ()]]
volumeLs2Widgets s = [[txt "NAME", txt "MOUNT POINT", txt "DRIVER", txt "SIZE"]] ++ (map volume2Widget s)

volumeLs2Table :: VolumeLsInfo -> Table ()
volumeLs2Table s = setDefaultColAlignment AlignCenter $ rowBorders False $ table $ volumeLs2Widgets s

networkLs2Widgets :: NetworkLsInfo -> [[Widget ()]]
networkLs2Widgets s = [[txt "ID", txt "NAME", txt "DRIVER", txt "SCOPE", txt "CREATED AT"]] ++ (map image2Widget s)

networkLs2Table :: NetworkLsInfo -> Table ()
networkLs2Table s =  setDefaultColAlignment AlignCenter $ rowBorders False $ table $ networkLs2Widgets s

mainMenu :: ContainerLsInfo -> ImageLsInfo -> VolumeLsInfo -> NetworkLsInfo -> IO Command
mainMenu containers images volumes networks = M.defaultMain (getApp containers images volumes networks) DockerMain

testMainMenu :: IO ()
testMainMenu = do 
    containers <- psCmd
    images <- imageLsCmd
    volumes <- volumeLsCmd
    networks <- networkLsCmd
    case (containers, images, volumes, networks) of 
        (Left ex, _, _, _) -> putStrLn $ "Error: " ++ ex
        (_, Left ex, _, _) -> putStrLn $ "Error: " ++ ex
        (_, _, Left ex, _) -> putStrLn $ "Error: " ++ ex
        (_, _, _, Left ex) -> putStrLn $ "Error: " ++ ex
        (Right c, Right i, Right v, Right n) -> do 
            res <- mainMenu c i v n
            putStrLn $ "You chose: " <> show res

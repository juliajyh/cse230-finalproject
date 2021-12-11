{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Backend
  ( Command (..),
    ContainerLsInfo,
    ImageLsInfo,
    NetworkLsInfo,
    VolumeLsInfo,
    imageLsCmd,
    networkLsCmd,
    psCmd,
    volumeLsCmd,
  )
import Brick.AttrMap
  ( attrMap,
  )
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types
  ( ViewportType (Both, Horizontal, Vertical),
    Widget,
  )
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hBox,
    hLimit,
    str,
    updateAttrMap,
    vBox,
    vLimit,
    viewport,
  )
import Control.Monad (void)
import qualified Graphics.Vty as V

data Name
  = IVP1
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

getUI :: Container -> Image -> Volume -> Network -> Command -> [Widget Name]
getUI cs is vs ns = const [ui]
  where
    ui =
      C.center $
        B.border $
          hLimit 150 $
            vLimit 40 $
              vBox
                [ str $
                    "Press 'r' to run container; Press 'i' to pull image; Press 'd' to remove container;\n"
                      <> "Press 'x' to remove image; Press 'e' to execute command; Press 'a' to start container;\n"
                      <> "Press 's' to stop container; Press <Enter> to refresh; Press <Esc> to quit",
                  str "abcde to scroll down, ABCDE to scroll up",
                  B.hBorder,
                  imageslist,
                  B.hBorder,
                  str "fghijk to scroll down, FGHIJK to scroll up",
                  B.hBorder,
                  containerslist,
                  B.hBorder,
                  str "lmno to scroll down, LMNO to scroll up",
                  B.hBorder,
                  volumeslist,
                  B.hBorder,
                  str "pqrst to scroll down, PQRST to scroll up",
                  B.hBorder,
                  networklist
                ]
    (il1, il2, il3, il4, il5) = is
    (cl1, cl2, cl3, cl4, cl5, cl6) = cs
    (vl1, vl2, vl3, vl4) = vs
    (nl1, nl2, nl3, nl4, nl5) = ns

    imageslist =
      vBox
        [ str "images",
          B.hBorder,
          hBox
            [ viewport IVP1 Vertical $
                vBox $
                  str "ID" :
                  str " " :
                  (str <$> il1),
              B.vBorder,
              viewport IVP2 Vertical $
                vBox $
                  str "Name" :
                  str " " :
                  (str <$> il2),
              B.vBorder,
              viewport IVP3 Vertical $
                vBox $
                  str "State" :
                  str " " :
                  (str <$> il3),
              B.vBorder,
              viewport IVP4 Vertical $
                vBox $
                  str "Status" :
                  str " " :
                  (str <$> il4),
              B.vBorder,
              viewport IVP5 Vertical $
                vBox $
                  str "Port" :
                  str " " :
                  (str <$> il5)
            ]
        ]

    containerslist =
      vBox
        [ str "containers",
          B.hBorder,
          hBox
            [ viewport CVP1 Vertical $
                vBox $
                  str "ID" :
                  str " " :
                  (str <$> cl1),
              B.vBorder,
              viewport CVP2 Vertical $
                vBox $
                  str "Name" :
                  str " " :
                  (str <$> cl2),
              B.vBorder,
              viewport CVP3 Vertical $
                vBox $
                  str "Image" :
                  str " " :
                  (str <$> cl3),
              B.vBorder,
              viewport CVP4 Vertical $
                vBox $
                  str "State" :
                  str " " :
                  (str <$> cl4),
              B.vBorder,
              viewport CVP5 Vertical $
                vBox $
                  str "Status" :
                  str " " :
                  (str <$> cl5),
              B.vBorder,
              viewport CVP6 Vertical $
                vBox $
                  str "Port" :
                  str " " :
                  (str <$> cl6)
            ]
        ]

    -- (Name, Mount_Point, Driver, Size)
    volumeslist =
      vBox
        [ str "volumes",
          B.hBorder,
          hBox
            [ viewport VVP1 Vertical $
                vBox $
                  str "Name" :
                  str " " :
                  (str <$> vl1),
              B.vBorder,
              viewport VVP2 Vertical $
                vBox $
                  str "Mount Point" :
                  str " " :
                  (str <$> vl2),
              B.vBorder,
              viewport VVP3 Vertical $
                vBox $
                  str "Driver" :
                  str " " :
                  (str <$> vl3),
              B.vBorder,
              viewport VVP4 Vertical $
                vBox $
                  str "Size" :
                  str " " :
                  (str <$> vl4)
            ]
        ]

    -- (ID, Name, Driver, Scope, Created)
    networklist =
      vBox
        [ str "network",
          B.hBorder,
          hBox
            [ viewport NVP1 Vertical $
                vBox $
                  str "ID" :
                  str " " :
                  (str <$> nl1),
              B.vBorder,
              viewport NVP2 Vertical $
                vBox $
                  str "Name" :
                  str " " :
                  (str <$> nl2),
              B.vBorder,
              viewport NVP3 Vertical $
                vBox $
                  str "Driver" :
                  str " " :
                  (str <$> nl3),
              B.vBorder,
              viewport NVP4 Vertical $
                vBox $
                  str "Scope" :
                  str " " :
                  (str <$> nl4),
              B.vBorder,
              viewport NVP5 Vertical $
                vBox $
                  str "Created" :
                  str " " :
                  (str <$> nl5)
            ]
        ]

-- handle image
pImagesCmd :: IO Image
pImagesCmd = do
  tuples <- genImagesCmd
  return $ tupToImagesList tuples

genImagesCmd :: IO [(String, String, String, String, String)]
genImagesCmd = return [("b78af7a83692", "fedora", "latest", "11 days ago", "153MB"), ("ba6acccedd29", "ubuntu", "latest", "7 weeks ago", "72.8MB"), ("d02e6b3a30c7", "hcyang99/snps16", "latest", "2 months ago", "15.1GB")]

tupToImagesList :: [(String, String, String, String, String)] -> Image
tupToImagesList [] = ([], [], [], [], [])
tupToImagesList (l : ls) = ((l1 : ls1), (l2 : ls2), (l3 : ls3), (l4 : ls4), (l5 : ls5))
  where
    (l1, l2, l3, l4, l5) = l
    (ls1, ls2, ls3, ls4, ls5) = tupToImagesList ls

-- handle container
pContainersCmd :: IO Container
pContainersCmd = do
  tuples <- genContainersCmd
  return $ tupToContainersList tuples

genContainersCmd :: IO [(String, String, String, String, String, String)]
genContainersCmd = return [("71f74b582fc0", "test", "fedora", "exited", "Exited (0) 2 hours ago", ""), ("118032700ff4", "frp", "ubuntu", "running", "Up 41 hours", "0.0.0.0:35600->35600/tcp, :::35600->35600/tcp, 0.0.0.0:35622->35622/tcp, :::35622->35622/tcp"), ("74194839b992", "pl", "ubuntu", "running", "Up 2 days", "")]

tupToContainersList :: [(String, String, String, String, String, String)] -> Container
tupToContainersList [] = ([], [], [], [], [], [])
tupToContainersList (l : ls) = ((l1 : ls1), (l2 : ls2), (l3 : ls3), (l4 : ls4), (l5 : ls5), (l6 : ls6))
  where
    (l1, l2, l3, l4, l5, l6) = l
    (ls1, ls2, ls3, ls4, ls5, ls6) = tupToContainersList ls

-- handle volume
pVolumesCmd :: IO Volume
pVolumesCmd = do
  tuples <- genVolumesCmd
  return $ tupToVolumesList tuples

genVolumesCmd :: IO [(String, String, String, String)]
genVolumesCmd = return [("data", "/var/lib/docker/volumes/data/_data", "local", "N/A")]

tupToVolumesList :: [(String, String, String, String)] -> Volume
tupToVolumesList [] = ([], [], [], [])
tupToVolumesList (l : ls) = ((l1 : ls1), (l2 : ls2), (l3 : ls3), (l4 : ls4))
  where
    (l1, l2, l3, l4) = l
    (ls1, ls2, ls3, ls4) = tupToVolumesList ls

-- handle network
pNetworkCmd :: IO Network
pNetworkCmd = do
  tuples <- genNetworkCmd
  return $ tupToNetworkList tuples

genNetworkCmd :: IO [(String, String, String, String, String)]
genNetworkCmd = return [("60d0dc8d3108", "bridge", "bridge", "local", "2021-12-06 16:29:32.005620093 -0800 PST"), ("9474ae3b0db9", "host", "host", "local", "2021-12-06 16:29:31.978428526 -0800 PST"), ("1f6332bc0c50", "none", "null", "local", "2021-12-06 16:29:31.965261296 -0800 PST")]

tupToNetworkList :: [(String, String, String, String, String)] -> Network
tupToNetworkList [] = ([], [], [], [], [])
tupToNetworkList (l : ls) = ((l1 : ls1), (l2 : ls2), (l3 : ls3), (l4 : ls4), (l5 : ls5))
  where
    (l1, l2, l3, l4, l5) = l
    (ls1, ls2, ls3, ls4, ls5) = tupToNetworkList ls

-- appEvent :: () -> T.BrickEvent Name e -> T.EventM Name (T.Next ())
appEvent :: Command -> T.BrickEvent Name e -> T.EventM Name (T.Next Command)
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

app :: Container -> Image -> Volume -> Network -> M.App Command e Name
app c i v n =
  M.App
    { M.appDraw = getUI c i v n,
      M.appStartEvent = return,
      M.appHandleEvent = appEvent,
      M.appAttrMap = const $ attrMap V.defAttr [],
      M.appChooseCursor = M.neverShowCursor
    }

initialState :: Command
initialState = DockerMain

mainMenu :: Container -> Image -> Volume -> Network -> IO Command
mainMenu c i v n = M.defaultMain (app c i v n) DockerMain

-- runMainMenu :: IO ()
-- runMainMenu = do
--     containers <- psCmd
--     images <- imageLsCmd
--     volumes <- volumeLsCmd
--     networks <- networkLsCmd
--     case (containers, images, volumes, networks) of
--         (Left ex, _, _, _) -> resultDialog "Error" ex
--         (_, Left ex, _, _) -> resultDialog "Error" ex
--         (_, _, Left ex, _) -> resultDialog "Error" ex
--         (_, _, _, Left ex) -> resultDialog "Error" ex
--         (Right c, Right i, Right v, Right n) -> do
--             cmd <- mainMenu c i v n
--             case cmd of
--                 DockerMain -> runMainMenu
--                 DockerImagePull -> runDockerImagePull UIDockerPull.initialDockerImageInfo
--                 DockerImageRm  -> runDockerImageRm UIDockerImageRm.initialDockerImageInfo
--                 DockerRun -> runDockerRun R.initialDockerRunInfo
--                 DockerRm -> runDockerRm UIDockerContainerRm.initialDockerContainerInfo
--                 DockerExec -> runDockerExec UIDockerExec.initialDockerExecInfo
--                 DockerStart -> runDockerStart UIDockerStart.initialDockerContainerInfo
--                 DockerStop -> runDockerStop UIDockerStop.initialDockerContainerInfo
--                 DockerHalt -> return ()

main :: IO ()
main = do
  containers <- psCmd
  images <- imageLsCmd
  volumes <- volumeLsCmd
  networks <- networkLsCmd
  case (containers, images, volumes, networks) of
    (Left ex, _, _, _) -> putStrLn $ "Error: " ++ ex
    (_, Left ex, _, _) -> putStrLn $ "Error: " ++ ex
    (_, _, Left ex, _) -> putStrLn $ "Error: " ++ ex
    (_, _, _, Left ex) -> putStrLn $ "Error: " ++ ex
    (Right c, Right i, Right v, Right n) ->
      putStrLn $ "You chose: " <> show c

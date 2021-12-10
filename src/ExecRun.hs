module ExecRun(execRun, testExecRun) where

import Control.Monad(void)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Types(Widget, BrickEvent)
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Themes
  ( Theme
  , newTheme
  , themeToAttrMap
  )
import Brick.Util (on, fg)
import Brick.AttrMap (AttrName)
import Backend(runCmd)
import Control.Monad.IO.Class (MonadIO(liftIO))
import ResultDialog (resultDialog)

type RunArgs = (String, String, [(String, String)], [(String, String)], String, Bool, Bool, Bool)

theme1 :: Theme
theme1 = newTheme (V.white `on` V.blue) []

getUI :: String -> String -> [Widget ()]
getUI image name = [ui]
    where
        ui = center $ hLimit 60 $ vBox $ hCenter <$>
            [
                str $ "Run container \"" ++ name ++ "\"",
                str $ "Using image \"" ++ image ++ "\"?",
                str " ",
                str "Press <Enter> to continue; Press <Esc> to cancel",
                str " ",
                str "Please Wait ..."
            ]

getAppEvent :: RunArgs -> String -> BrickEvent () e -> T.EventM () (T.Next String)
getAppEvent args s (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt "Cancelled"
        V.EvKey V.KEnter [] -> do 
            val <- liftIO $ runCmd args
            case val of
                Left ex -> M.halt ex
                Right res -> M.halt $ "Success: " ++ res
        _ -> M.continue s
getAppEvent args s _ = M.continue s


getApp :: RunArgs -> M.App String e ()
getApp args@(image, name, mounts, ports, command, attach, volatile, daemon) =
    M.App { M.appDraw = const $ getUI image name
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = getAppEvent args
          , M.appStartEvent = return
          , M.appAttrMap = \_ -> themeToAttrMap theme1
          }

defaultRunArgs :: RunArgs
defaultRunArgs = ("hcyang99/snps16", "cad", [("/home/hcyang/Documents/source/repos/patternet", "/mnt/repos/patternet"), ("/home/hcyang/Documents/source/env/snps16/.vscode-server/", "/root/.vscode-server")], [("80", "80"), ("443", "443")], "uname -r", False, True, False)

execRun :: RunArgs -> IO String 
execRun args = M.defaultMain (getApp args) ""

testExecRun :: IO ()
testExecRun = do
    s <- execRun defaultRunArgs
    resultDialog "Run Container" s

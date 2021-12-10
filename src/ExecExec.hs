module ExecExec(execExec, testExecExec) where

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
import Backend(startCmd, execCmd)
import Control.Monad.IO.Class (MonadIO(liftIO))
import ResultDialog (resultDialog)

theme1 :: Theme
theme1 = newTheme (V.white `on` V.blue) []

getUI :: String -> String -> [Widget ()]
getUI name command = [ui]
    where
        ui = center $ hLimit 60 $ vBox $ hCenter <$>
            [
                str $ "Run command \"" ++ command ++ "\"",
                str $ "On container \"" ++ name ++ "\"?",
                str " ",
                str "Press <Enter> to continue; Press <Esc> to cancel",
                str " ",
                str "Please Wait ..."
            ]

getAppEvent :: String -> String -> String -> BrickEvent () e -> T.EventM () (T.Next String)
getAppEvent name command s (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt "Cancelled"
        V.EvKey V.KEnter [] -> do 
            val <- liftIO $ execCmd name command
            case val of
                Left ex -> M.halt ex
                Right res -> M.halt $ "Success: " ++ res
        _ -> M.continue s
getAppEvent _ _ s _ = M.continue s


getApp :: String -> String -> M.App String e ()
getApp name command =
    M.App { M.appDraw = const $ getUI name command
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = getAppEvent name command
          , M.appStartEvent = return
          , M.appAttrMap = \_ -> themeToAttrMap theme1
          }

execExec :: String -> String -> IO String 
execExec name command = M.defaultMain (getApp name command) ""

testExecExec :: IO ()
testExecExec = do
    s <- execExec "pl" "uname -r"
    resultDialog "Run Command" s

module ExecStop(execStop, testExecStop) where

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
import Backend(stopCmd)
import Control.Monad.IO.Class (MonadIO(liftIO))

theme1 :: Theme
theme1 = newTheme (V.white `on` V.blue) []

drawUI :: String -> [Widget ()]
drawUI name = [ui]
    where
        ui = center $ hLimit 60 $ vBox $ hCenter <$>
            [
                str $ "Stop container \"" ++ name ++ "\"?",
                str " ",
                str "Press <Enter> to continue; Press <Esc> to cancel",
                str " ",
                str "Please Wait ..."
            ]

appEvent :: String -> BrickEvent () e -> T.EventM () (T.Next String)
appEvent s (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt "Cancelled"
        V.EvKey V.KEnter [] -> do 
            val <- liftIO $ stopCmd s
            case val of
                Left ex -> M.halt ex
                Right res -> M.halt $ "Successfully stopped container \"" ++ s ++ "\"\n"
        _ -> M.continue s
appEvent s _ = M.continue s


app :: M.App String e ()
app =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = \_ -> themeToAttrMap theme1
          }

execStop :: IO String 
execStop = M.defaultMain app "fedora"

testExecStop :: IO ()
testExecStop = do
    s <- execStop
    print s

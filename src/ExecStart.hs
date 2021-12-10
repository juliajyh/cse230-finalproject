module ExecStart(execStart, testExecStart) where

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
import Backend(startCmd)
import Control.Monad.IO.Class (MonadIO(liftIO))
import ResultDialog (resultDialog)

theme1 :: Theme
theme1 = newTheme (V.white `on` V.blue) []

drawUI :: String -> [Widget ()]
drawUI name = [ui]
    where
        ui = center $ hLimit 60 $ vBox $ hCenter <$>
            [
                str $ "Start container \"" ++ name ++ "\"?",
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
            val <- liftIO $ startCmd s
            case val of
                Left ex -> M.halt ex
                Right res -> M.halt $ "Success: " ++ res
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

execStart :: IO String 
execStart = M.defaultMain app "tst"

testExecStart :: IO ()
testExecStart = do
    s <- execStart
    resultDialog "Start Container" s

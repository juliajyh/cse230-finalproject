{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UIDockerRun(testUIDockerRun, uiDockerRun, initialDockerRunInfo, DockerRunInfo) where

import qualified Data.Text as T
import Lens.Micro ((^.))
import Lens.Micro.TH
import qualified Graphics.Vty as V
import Brick
    ( BrickEvent(VtyEvent),
      Widget,
      App(..),
      AttrMap,
      attrMap,
      continue,
      customMain,
      halt,
      on,
      (<+>),
      (<=>),
      fill,
      hLimit,
      padBottom,
      padTop,
      str,
      vLimit,
      Padding(Pad) )
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

data Name = ImageField
          | NameField
          | MountField
          | PortField
          | CommandField
          | AttachField
          | VolatileField
          | DaemonField
          deriving (Eq, Ord, Show)

data DockerRunInfo =
    DockerRunInfo { _image       :: T.Text 
             , _name        :: T.Text 
             , _mounts      :: T.Text 
             , _ports       :: T.Text 
             , _command     :: T.Text 
             , _attach      :: Bool 
             , _volatile    :: Bool 
             , _daemon      :: Bool
             , _cancel      :: Bool
             }
             deriving (Show)

makeLenses ''DockerRunInfo

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: DockerRunInfo -> Form DockerRunInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Image" @@=
                   editTextField image ImageField (Just 1)
               , label "Name" @@=
                   editTextField name NameField (Just 1)
               , label "Mount" @@=
                 B.borderWithLabel (str "<host-path>:<guest-path>; Separated by NewLine or Semicolon") @@=
                   editTextField mounts MountField (Just 3)
               , label "Ports" @@=
                 B.borderWithLabel (str "<host-port>:<guest-port>; Separated by Comma") @@=
                   editTextField ports PortField (Just 1)
               , label "Command" @@=
                   editTextField command CommandField (Just 1)
               , label "" @@=
                   checkboxField attach AttachField "Attach"
               , label "" @@=
                   checkboxField volatile VolatileField "Volatile"
               , label "" @@=
                   checkboxField daemon DaemonField "Daemon"
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.white `on` V.blue)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.white `on` V.blue)
  ]

draw :: Form DockerRunInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.borderWithLabel (str "Run Container") $ padTop (Pad 1) $ hLimit 80 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Press <Enter> to continue; Press <Esc> to exit\n" <>
                     "- Mount Example 1:\n" <> 
                     "      /home/user/host-dir:/mnt/guest-dir; /home/user/dir2:/mnt/dir2 \n" <>
                     "      /home/user/dir3:/mnt/dir3\n" <>
                     "- Mount Example 2:\n" <>
                     "      /home/user/host-dir:/mnt/guest-dir\n" <>
                     "      /home/user/dir2:/mnt/dir2\n" <>
                     "- Ports Example 1: 80:80\n" <>
                     "- Ports Example 2: 80:80, 443:443"


app :: App (Form DockerRunInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt $ mkForm (formState s){_cancel = True}
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just MountField -> 
                        halt $ mkForm (formState s){_cancel = False }
                _ -> do 
                    s' <- handleFormEvent ev s
                    continue s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }


initialDockerRunInfo :: DockerRunInfo
initialDockerRunInfo = DockerRunInfo { _image = ""
                            , _name = ""
                            , _mounts = ""
                            , _ports = ""
                            , _command = ""
                            , _attach = False 
                            , _volatile = False 
                            , _daemon = False
                            , _cancel = False
                            }

uiDockerRun :: DockerRunInfo -> IO DockerRunInfo
uiDockerRun oldInfo = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        f = mkForm oldInfo

    initialVty <- buildVty 
    f' <- customMain initialVty buildVty Nothing app f
    return $ formState f'

testUIDockerRun :: IO ()
testUIDockerRun = do
    newInfo <- uiDockerRun initialDockerRunInfo

    putStrLn "The starting form state was:"
    print initialDockerRunInfo

    putStrLn "The final form state was:"
    print newInfo
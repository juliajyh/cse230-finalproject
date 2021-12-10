{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UIDockerImageRm(testUIDockerImageRm, uiDockerImageRm, initialDockerImageInfo, DockerImageInfo, getImage, getCancel) where

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
          deriving (Eq, Ord, Show)

data DockerImageInfo =
    DockerImageInfo { _image       :: T.Text 
                   ,_cancel      :: Bool
             }
             deriving (Show)

makeLenses ''DockerImageInfo

getImage :: DockerImageInfo -> T.Text
getImage info = _image info

getCancel :: DockerImageInfo -> Bool 
getCancel info = _cancel info

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: DockerImageInfo -> Form DockerImageInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Image" @@=
                   editTextField image ImageField (Just 1)
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.white `on` V.blue)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.white `on` V.blue)
  ]

draw :: Form DockerImageInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.borderWithLabel (str "Remove Image") $ padTop (Pad 1) $ hLimit 80 $ renderForm f
        help = padTop (Pad 1) body
        body = str "Press <Enter> to continue; Press <Esc> to exit"

app :: App (Form DockerImageInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt $ mkForm (formState s){_cancel = True}
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter []) -> halt $ mkForm (formState s){_cancel = False }
                _ -> do 
                    s' <- handleFormEvent ev s
                    continue s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }


initialDockerImageInfo :: DockerImageInfo
initialDockerImageInfo = DockerImageInfo { _image = ""
                                      ,_cancel = False
                            }

uiDockerImageRm :: DockerImageInfo -> IO DockerImageInfo
uiDockerImageRm oldInfo = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        f = mkForm oldInfo

    initialVty <- buildVty 
    f' <- customMain initialVty buildVty Nothing app f
    return $ formState f'

testUIDockerImageRm :: IO ()
testUIDockerImageRm = do
    newInfo <- uiDockerImageRm initialDockerImageInfo

    putStrLn "The starting form state was:"
    print initialDockerImageInfo

    putStrLn "The final form state was:"
    print newInfo
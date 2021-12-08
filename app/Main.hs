{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

import Text.Parsec
import Text.Parsec.String
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimitPercent, str, hBox, vLimitPercent, withAttr, (<+>), (<=>))
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lib



drawUI :: Widget() -> [Widget ()]
drawUI uilists = [ui]
      where
      ui = C.vCenter $
          C.hCenter $
          B.borderWithLabel (str "images") $
          hLimitPercent 20 $
          vLimitPercent 90 $
          C.center (str "")
          <=> C.vCenter $
          C.hCenter $
          B.borderWithLabel (str "containers") $
          hLimitPercent 20 $
          vLimitPercent 90 uilists
           <=> C.vCenter
          $ C.hCenter $
          B.borderWithLabel (str "volumes") $
          hLimitPercent 20 $
          vLimitPercent 90 $
          C.center (str "")
          <=> C.vCenter $
          C.hCenter $
          B.borderWithLabel (str "images") $
          hLimitPercent 20 $
          vLimitPercent 90 $
          C.center (str "")

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a = C.hCenter (str s)


-- ("Names", "Image", "State", "Status", "Ports")
-- uihelper ::  -> Widget ()
-- uihelper psCmds = do
--   cmds <- psCmds
--     case cmds of
--       Left _ ->
--         C.hCenter $
--           hBox
--             [ B.hBorderWithLabel $ str "Names",
--               B.hBorderWithLabel $ str "Image",
--               B.hBorderWithLabel $ str "State",
--               B.hBorderWithLabel $ str "Status",
--               B.hBorderWithLabel $ str "Ports"
--             ]
--       Right tuples -> do

convert :: [(String, String, String, String, String)] -> IO ([String], [String], [String], [String], [String])
convert [] = return ([],[],[],[],[])
convert (t:ts) = do
  (l1, l2, l3, l4, l5) <- convert ts
  let (e1, e2, e3, e4, e5) = t in return (e1:l1, e2:l2, e3:l3, e4:l4, e5:l5)

isNotSemiColon :: Char -> Bool
isNotSemiColon = isNotChars [';', '\n']
  

fparser :: Parser [(String, String, String, String, String)]
fparser = do
  e1 <- many isNotSemiColon
  _ <- char ';'
  e2 <- many isNotSemiColon
  _ <- char ';'
  e3 <- many isNotSemiColon
  _ <- char ';'
  e4 <- many isNotSemiColon
  _ <- char ';'
  e5 <- many isNotSemiColon
  _ <- char ';'
  _ <- endOfLine
  lrest <- fparser
  return (e1, e2, e3, e4, e5):lrest
                  

appEvent :: IO [(String, String, String, String, String)] -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () Char))
-- appEvent l (T.VtyEvent e) =
--     case e of
--         V.EvKey (V.KChar '+') [] ->
--             let el = nextElement (L.listElements l)
--                 pos = Vec.length $ l^.(L.listElementsL)
--             in M.continue $ L.listInsert pos el l

--         V.EvKey (V.KChar '-') [] ->
--             case l^.(L.listSelectedL) of
--                 Nothing -> M.continue l
--                 Just i  -> M.continue $ L.listRemove i l

--         V.EvKey V.KEsc [] -> M.halt l

--         ev -> M.continue =<< (L.handleListEventVi L.handleListEvent) ev l
--     where
--       nextElement :: Vec.Vector Char -> Char
--       nextElement v = fromMaybe '?' $ Vec.find (flip Vec.notElem v) (Vec.fromList ['a' .. 'z'])

appEvent l _ = M.continue l


initialState :: Widget()
initialState = do
  ts <- parseFromFile fparser "test/in/input.imp"
  (l1, l2, l3, l4, l5) <- convert ts
  C.hCenter $
    hBox
      [ B.hBorderWithLabel (str "Names") $ L.renderList listDrawElement True $ L.list () (Vec.fromList l1) 1,
        B.hBorderWithLabel (str "Image") $ L.renderList listDrawElement True $ L.list () (Vec.fromList l2) 1, 
        B.hBorderWithLabel (str "State") $ L.renderList listDrawElement True $ L.list () (Vec.fromList l3) 1,
        B.hBorderWithLabel (str "Status") $ L.renderList listDrawElement True $ L.list () (Vec.fromList l4) 1,
        B.hBorderWithLabel (str "Ports") $ L.renderList listDrawElement True $ L.list () (Vec.fromList l5) 1
      ]
        
        


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (Widget ()) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState

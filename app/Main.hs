{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, hBox, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as F
import qualified Data.Vector as Vec

data Name = TOP
          | BOTTOM
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       ,_topList :: L.List Name [String]
       , _bottomList :: L.List Name [String]
       }

makeLenses ''St

-- e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
--         e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)

drawUI :: St -> [Widget ()]
drawUI st = [ui]
    where
        box1 = B.borderWithLabel (str "list1 ") $
              hLimit 20 $
              vLimit 20 $
              F.withFocusRing (st^.focusRing) (L.renderList listDrawElement) (st^.topList)

        box2 = B.borderWithLabel (str "list2 ") $
              hLimit 90 $
              vLimit 50 $
              F.withFocusRing (st^.focusRing) (L.renderList listDrawElement) (st^.bottomList)
        ui = C.vCenter $ vBox [ C.hCenter box1
                              , C.hCenter box2
                              ]
        
    -- • Couldn't match type ‘Name’ with ‘()’
    --   Expected type: L.GenericList () Vec.Vector [String]
    --     Actual type: L.List Name [String]

appEvent :: St -> T.BrickEvent () e -> T.EventM () (T.Next St)
appEvent st (T.VtyEvent e) =
    case e of

        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
        V.EvKey (V.KChar '+') [] -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just TOP -> 
                  let el = nextElementTop (L.listElements (st^.topList))
                      pos = Vec.length $ (st^.topList)^.(L.listElementsL)
                  in return $ st & topList %~ (\l -> (L.listInsert pos el l))

               Just BOTTOM -> 
                   let el = nextElementBottom (L.listElements (st^.bottomList))
                       pos = Vec.length $ (st^.bottomList)^.(L.listElementsL)
                    in return $ st & bottomList %~ (\l -> (L.listInsert pos el l))

               Nothing -> return st

        V.EvKey (V.KChar '-') [] -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
              Just TOP ->
                case (st^.topList)^.(L.listSelectedL) of
                  Nothing -> return $ st & topList %~ (\l -> l)
                  Just i  -> return $ st & topList %~ (\l -> L.listRemove i l)
              
              Just BOTTOM ->
                case (st^.bottomList)^.(L.listSelectedL) of
                  Nothing -> return $ st & bottomList %~ (\l -> l)
                  Just i  -> return $ st & bottomList %~ (\l -> L.listRemove i l)

              Nothing -> return st

        V.EvKey V.KEsc [] -> M.halt st

        ev -> M.continue =<< return st
    where
      nextElementTop :: Vec.Vector [String] -> [String]
      nextElementTop v = fromMaybe ["?", "??", "???"] $ Vec.find (flip Vec.notElem v) (Vec.fromList [["Name", "Images", "Containers"], ["hello-word", "backend", "mysql"]])
      
      nextElementBottom :: Vec.Vector [String] -> [String]
      nextElementBottom v = fromMaybe ["?", "??", "???"] $ Vec.find (flip Vec.notElem v) (Vec.fromList [["ID", "Name", "Port"], ["7655909", "hello-world", "8888"]])


appEvent st _ = M.continue st


listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

initialState :: St
initialState = St (F.focusRing [TOP, BOTTOM])
                  (L.list TOP (Vec.fromList [["Name", "Images", "Containers"], ["hello-word", "backend", "mysql"]]) 1)
                   (L.list BOTTOM (Vec.fromList [["ID", "Name", "Port"], ["7655909", "hello-world", "8888"]]) 1)   

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]


theApp :: M.App (St) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState

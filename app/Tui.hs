{-# LANGUAGE TypeApplications, DataKinds, RecordWildCards, DeriveGeneric, OverloadedStrings #-}

module Tui (
  app, update, draw, AppState(..)
           ) where


import Logic

import Brick hiding (on)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Data.Function
import Data.Time
import Data.Generics.Product hiding (list)
import GHC.Generics
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K
import Lens.Micro

-- TUI

data AppState
  = AppState 
    { todos         :: [Todo]
    , currentTime   :: UTCTime
    , addTodoEditor :: Editor String String
    , editorActive  :: Bool
    }
  deriving Generic

app :: App AppState (UTCTime, [Todo]) String
app =
  App
  { appDraw = draw
  , appChooseCursor = const.const Nothing
  , appHandleEvent  = update
  , appStartEvent   = pure
  , appAttrMap      = const (attrMap defAttr 
    [ (attrName "infoTitle", fg magenta)
    ])
  }

draw :: AppState -> [Widget String]
draw s = 
  [ borderWithLabel (center $ title "Odot") $ vBox
    [ {- border $ padAll 1 $
      htitle "Add a todo: " <+> renderEditor drawEditor True (addTodoEditor s)
      -}
      padAll 1 $ hCenter $ border $ hCenter $ padTopBottom 1 $ str "WIP" 

    , hCenter $ title "Here are your todos:"
    
    , padAll 1 $ withClickableVScrollBars (const id) $ 
      withVScrollBarHandles $ withVScrollBars OnRight $
      viewport "TodosViewport" Vertical $
      
      vBox $ map drawTodo $ sortTodos (currentTime s) (todos s)
    ]
  ]
    where
      title t =
        withAttr "infoTitle" $
        txt t

      drawEditor = foldl1 (<+>).map str

      drawDue _ (Just t) = str ("due "  <> showTime t)
      drawDue t Nothing  = str ("from " <> showTime t)


      drawTodo Todo {..} = border $ padAll 1 $
        str todoName <+> strWrap " " <+> drawDue todoDate todoDue

update :: AppState -> BrickEvent String (UTCTime, [Todo]) -> EventM String (Next AppState)

update s (AppEvent (newtime, todos)) = s 
  & field @"currentTime" .~ newtime 
  & field @"todos"       .~ todos
  & continue

update s (VtyEvent (V.EvKey (K.KChar 'q') []))
  | s & not.editorActive = halt s

{- 
update s (VtyEvent (V.EvKey (K.KChar 'a') []))
  | s & not.editorActive 
  = s
  & field @"editorActive" .~ True
  & continue
-}

update s (VtyEvent (V.EvKey K.KEnter      [])) = s
  & field @"editorActive" .~ False
  & continue

update s (VtyEvent (V.EvKey K.KUp         [])) = 
  vScrollBy (viewportScroll "TodosViewport") (-1) >> continue s

update s (VtyEvent (V.EvKey K.KDown       [])) = 
  vScrollBy (viewportScroll "TodosViewport")   1  >> continue s


update s (VtyEvent e)
  | editorActive s = do

  editor' <- s 
    & addTodoEditor 
    & handleEditorEvent e
  s 
    & field @"addTodoEditor" .~ editor' 
    & continue

update s _ = continue s


